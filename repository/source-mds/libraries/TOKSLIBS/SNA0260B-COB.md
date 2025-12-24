# SNA0260B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0260B.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                  *
*    業務名　　　　　　　：　                                *
*    モジュール名　　　　：　HULFT排他制御チェック           *
*    作成日／更新日　　　：　12/04/12                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　                    *
*    処理概要　　　　　　：　HULFT排他制御ファイルを取得し   *
*                            上位へ返却する。　　　　　　    *
*                                                            *
**************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0260B.
 AUTHOR.               MIURA.
 DATE-WRITTEN.         12/04/12.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*排他制御ファイル
     SELECT  NARLOCF  ASSIGN TO    NARLOCF
             ORGANIZAITION         SEQUENTIAL
             ACCESS      MODE      SEQUENTIAL
                         FILE      STATUS    NAR-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 排他制御ファイル　　　　　　　　　             *
****************************************************************
 FD  NARLOCF           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARLOCF   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  NAR-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  NAR-ERR           PIC N(13) VALUE
         NC"排他制御ファイル　　エラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
    03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
****************************************************************
 LINKAGE              SECTION.
****************************************************************
* パラメーター エリア
 01  PARA-UMKBN                   PIC  X(01).
 01  PARA-BMNCD                   PIC  X(04).
 01  PARA-TANCD                   PIC  X(02).
 01  PARA-WKSTN                   PIC  X(08).
 01  PARA-RENNO                   PIC  X(09).
 01  PARA-ONLTGK                  PIC  X(01).
*
**************************************************************
 PROCEDURE         DIVISION   USING  PARA-UMKBN PARA-BMNCD
                                     PARA-TANCD PARA-WKSTN
                                     PARA-RENNO PARA-ONLTGK.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARLOCF.
     MOVE        NAR-ST     TO       E-ST.
     MOVE       "NARLOCF "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT        NARLOCF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------排他制御ファイルの読み込み
     READ  NARLOCF
           AT END
           MOVE  "0"                TO   PARA-UMKBN
           MOVE  SPACE              TO   PARA-BMNCD
           MOVE  SPACE              TO   PARA-TANCD
           MOVE  SPACE              TO   PARA-WKSTN
           MOVE  SPACE              TO   PARA-RENNO
           MOVE  SPACE              TO   PARA-ONLTGK
           GO  TO  MAIN-EXIT
     END-READ.
     MOVE  "1"                TO   PARA-UMKBN.
     MOVE  NAR-F01            TO   PARA-BMNCD.
     MOVE  NAR-F02            TO   PARA-TANCD.
     MOVE  NAR-F03            TO   PARA-WKSTN.
     MOVE  NAR-F04            TO   PARA-RENNO.
     MOVE  NAR-F05            TO   PARA-ONLTGK.
*
 MAIN-EXIT.
     EXIT.
**************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのCLOSE処理
     CLOSE     NARLOCF.
*
 END-EXIT.
     EXIT.

```
