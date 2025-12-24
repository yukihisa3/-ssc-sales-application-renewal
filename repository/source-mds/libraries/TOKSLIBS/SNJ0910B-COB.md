# SNJ0910B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0910B.COB`

## ソースコード

```cobol
**************************************************************
*                                                            *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    サブシステム　　　　：　受配信管理システム              *
*    業務名　　　　　　　：　ＥＯＳ管理                      *
*    モジュール名　　　　：　受信状況照会排他制御更新　 　　 *
*    作成日／更新日　　　：　12/05/08                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　                    *
*    処理概要　　　　　　：　発注集計表排他制御ファイルの　　*
*                            フラグを更新する。　　　　　    *
*                                                            *
**************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0910B.
 AUTHOR.               MIURA.
 DATE-WRITTEN.         12/05/08.
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
     SELECT  HACHLOCF  ASSIGN TO    DA-01-VI-HACHLOC1
             ORGANIZATION      IS   INDEXED
             ACCESS    MODE    IS   RANDOM
             RECORD    KEY     IS   LOC-F01
             FILE      STATUS  IS   LOC-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 排他制御ファイル　　　　　　　　　             *
****************************************************************
 FD  HACHLOCF           LABEL     RECORD    IS   STANDARD.
                       COPY      HACHLOCF   OF   XFDLIB
                       JOINING   LOC       AS   PREFIX.
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
     03  LOC-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  LOC-ERR           PIC N(13) VALUE
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
 01  PARA-KBN                     PIC  X(02).
 01  PARA-WKSTN                   PIC  X(08).
 01  PARA-RTN                     PIC  X(02).
*
**************************************************************
 PROCEDURE      DIVISION   USING  PARA-KBN PARA-WKSTN PARA-RTN.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACHLOCF.
     MOVE        LOC-ST      TO       E-ST.
     MOVE       "HACHLOCF "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     LOC-ERR   UPON      CONS.
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
     OPEN      I-O          HACHLOCF.
     ACCEPT    YMD02        FROM   DATE.
     ACCEPT    JIKAN        FROM   TIME.
     MOVE      SPACE   TO   PARA-RTN.
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------排他制御ファイルの読み込み
     IF  PARA-KBN = "ON"
         MOVE    SPACE     TO     LOC-REC
         INITIALIZE               LOC-REC
         MOVE  PARA-WKSTN         TO   LOC-F01
         MOVE  1                  TO   LOC-F02
         MOVE  HZK                TO   LOC-F98
         MOVE  JIKAN01            TO   LOC-F99
         WRITE LOC-REC
     ELSE
         MOVE  PARA-WKSTN         TO   LOC-F01
         READ  HACHLOCF
              INVALID KEY
                  GO  TO          MAIN-EXIT
              NOT INVALID KEY
                  DELETE          HACHLOCF
                  MOVE  "ON"  TO  PARA-RTN
         END-READ
     END-IF.
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
     CLOSE     HACHLOCF.
*
 END-EXIT.
     EXIT.

```
