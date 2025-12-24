# SNA0290B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0290B.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                  *
*    業務名　　　　　　　：　                                *
*    モジュール名　　　　：　HULFT排他制御更新（疎通フラグ） *
*    作成日／更新日　　　：　12/04/12                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　                    *
*    処理概要　　　　　　：　HULFT排他制御ファイルの疎通　   *
*                            フラグを更新する。　　　　　    *
*                                                            *
**************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0290B.
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
     SELECT  NARLOCF2  ASSIGN TO    NARLOCF2
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
 FD  NARLOCF2           LABEL     RECORD    IS   STANDARD.
                       COPY      NARLOCF2   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  HZK.
     03  YMD01                 PIC  9(02)     VALUE   20.
     03  YMD02                 PIC  9(06)     VALUE   ZERO.
 01  JIKAN.
     03  JIKAN01               PIC  9(06)     VALUE   ZERO.
     03  JIKAN02               PIC  9(02)     VALUE   ZERO.
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
*
**************************************************************
 PROCEDURE         DIVISION.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARLOCF2.
     MOVE        NAR-ST     TO       E-ST.
     MOVE       "NARLOCF2 "  TO       E-FILE.
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
     OPEN      I-O          NARLOCF2.
     ACCEPT    YMD02        FROM   DATE.
     ACCEPT    JIKAN        FROM   TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------排他制御ファイルの読み込み
     READ  NARLOCF2  AT   END
           GO TO    MAIN-EXIT
     END-READ.
     MOVE  1                  TO   NAR-F06.
     MOVE  HZK                TO   NAR-F98.
     MOVE  JIKAN01            TO   NAR-F99.
     REWRITE NAR-REC.
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
     CLOSE     NARLOCF2.
*
 END-EXIT.
     EXIT.

```
