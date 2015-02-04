// http://www.ne.jp/asahi/hishidama/home/tech/c/windows/CreatePipe.html
// i686-w64-mingw32-gcc -O -mms-bitfields -Wall -Wno-unused -o gege.exe   -I"C:/ocamlmgw/lib" "msvc.c" 

#include <stdio.h>
#include <windows.h>
HANDLE readPipe  = NULL;
HANDLE writePipe = NULL;

void printError(char *p)
{
    fprintf(stderr, "Error %s\n", p);
    exit(1);
}

int createPipe(void)
{
	HANDLE readTemp;

	// パイプを作成（両ハンドルとも子プロセスへ継承不可）
	if (!CreatePipe(&readTemp, &writePipe, NULL, 0)) {
		printError("CreatePipe");
		return -1;
	}

	// 読込ハンドルを複製（子プロセスへ継承可能な権限の読込ハンドルを作成）
	if (!DuplicateHandle(
		GetCurrentProcess(), readTemp,
		GetCurrentProcess(), &readPipe,
		0, TRUE, DUPLICATE_SAME_ACCESS)
	) {
		printError("DuplicateHandle");
		if (!CloseHandle(readTemp)) {
			printError("CloseHandle(readTemp)");
		}
		return -1;
	}

	// 複製元のハンドルは使わないのでクローズ
	if (!CloseHandle(readTemp)) {
		printError("CloseHandle(readTemp)");
		return -1;
	}

	return 0;
}
HANDLE childProcess = NULL;

int execute(LPTSTR commandLine)
{
	BOOL  bInheritHandles = TRUE;
	DWORD creationFlags   = 0;

	STARTUPINFO si = {};
	si.cb         = sizeof(STARTUPINFO);
	si.dwFlags    = STARTF_USESTDHANDLES;
	si.hStdInput  = readPipe;	//子プロセスの標準入力は読込用パイプにする
	si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE); //親プロセスの標準出力を継承
	si.hStdError  = GetStdHandle(STD_ERROR_HANDLE);  //親プロセスの標準エラーを継承

	if (si.hStdOutput == INVALID_HANDLE_VALUE) {
		printError("GetStdHandle(STD_OUTPUT_HANDLE)");
		return -1;
	}
	if (si.hStdError == INVALID_HANDLE_VALUE) {
		printError("GetStdHandle(STD_ERROR_HANDLE)");
		return -1;
	}

	// 子プロセスを起動
	PROCESS_INFORMATION pi = {};
	if (!CreateProcess(
		NULL,
		commandLine,
		NULL,	//プロセスのセキュリティー記述子
		NULL,	//スレッドのセキュリティー記述子
		bInheritHandles,
		creationFlags,
		NULL,	//環境変数は引き継ぐ
		NULL,	//カレントディレクトリーは同じ
		&si,
		&pi)
	) {
		printError("CreateProcess");
		return -1;
	}
	childProcess = pi.hProcess;
	if (!CloseHandle(pi.hThread)) {
		printError("CloseHandle(hThread)");
	}

	// 子プロセスを起動したら親プロセス側では読込ハンドルを使わないので、クローズする
	if(!CloseHandle(readPipe)) {
		printError("CloseHandle(readPipe)");
	}
	readPipe = NULL;

	// パイプへの書き込み（子プロセスへの送信）
	const char message[] = "example-pipe";
	DWORD numberOfBytesWritten;
	if (!WriteFile(writePipe, message, strlen(message), &numberOfBytesWritten, NULL)) {
		printError("WriteFile");
		return -1;
	}
	if(!CloseHandle(writePipe)) {
		printError("CloseHandle(writePipe)");
		return -1;
	}
	writePipe = NULL;

	// 子プロセスの終了待ち
	DWORD r = WaitForSingleObject(childProcess, INFINITE);
	switch(r) {
	case WAIT_FAILED:
		printError("wait result=WAIT_FAILED");
		return -1;
	case WAIT_ABANDONED:
		printf("wait result=WAIT_ABANDONED\n");
		return -1;
	case WAIT_OBJECT_0: //正常終了
		printf("wait result=WAIT_OBJECT_0\n");
		break;
	case WAIT_TIMEOUT:
		printf("wait result=WAIT_TIMEOUT\n");
		return -1;
	default:
		printf("wait result=%ld\n", r);
		return -1;
	}

	return 0;
}
void closeHandle(void)
{
	if (childProcess != NULL) {
		if(!CloseHandle(childProcess)) {
			printError("CloseHandle(childProcess)");
		}
	}
	if (readPipe != NULL) {
		if(!CloseHandle(readPipe)) {
			printError("CloseHandle(readPipe)");
		}
	}
	if (writePipe != NULL) {
		if(!CloseHandle(writePipe)) {
			printError("CloseHandle(writePipe)");
		}
	}
}
int main()
{
	printf("start\n");

	int r = createPipe();
	if (r == 0) {
		// LPTSTR commandLine = TEXT("child.exe");
		LPTSTR commandLine = TEXT("oco.exe");
		r = execute(commandLine);
	}
	closeHandle();

	printf("end\n");
	return r;
}

