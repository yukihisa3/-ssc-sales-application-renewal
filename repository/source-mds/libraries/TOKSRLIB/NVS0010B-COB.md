# NVS0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定表発行指示　　　　　　　　*
*    作成日／作成者　　　：　2020/02/22   ASS.II               *
*    処理内容　　　　　　：　実行制御マスタの存在チェック　　　*
*    　　　　　　　　　　　　を行う。　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0010B.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/02/22.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*実行制御マスタ
     SELECT  JSMJIKF   ASSIGN    TO        DA-01-VI-JSMJIKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JIK-F01
                       FILE      STATUS    JIK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 実行制御マスタ                                     *
****************************************************************
 FD  JSMJIKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMJIKF   OF   XFDLIB
                       JOINING   JIK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JIK-ST                   PIC  X(02).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JIK-ERR           PIC N(15) VALUE
                        NC"実行制御マスタエラー　".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 JIK-ERR-SEC           SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMJIKF.
     MOVE        JIK-ST    TO        E-ST.
     MOVE        "JSMJIKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JIK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***  ファイルのオープン
 P1.
     OPEN      INPUT    JSMJIKF.
***  実行制御マスタチェック
 P2.
     MOVE     "S"                 TO   JIK-F01.
     READ      JSMJIKF
               INVALID
               MOVE      "4010"   TO   PROGRAM-STATUS
               NOT  INVALID
               IF   JIK-F03   =  ZERO
                    MOVE   "4010" TO   PROGRAM-STATUS
               ELSE
                    MOVE   "4011" TO   PROGRAM-STATUS
               END-IF
     END-READ.
***  ファイルのクローズ
 P3.
     CLOSE     JSMJIKF.
***  プログラム終了
 P4.
     DISPLAY "STATUS-" PROGRAM-STATUS UPON CONS
***  STOP  RUN.
 PROCESS-EXD.
     EXIT.
*****************<<  NVS0010B   END PROGRAM  >>******************

```
