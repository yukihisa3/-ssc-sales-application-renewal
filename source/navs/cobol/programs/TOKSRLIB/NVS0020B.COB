****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　実行制御マスタＦＬＧセット　　　　*
*    作成日／作成者　　　：　2020/02/20   ASS.II               *
*    処理内容　　　　　　：　実行制御マスタの起動確認をて　　　*
*    　　　　　　　　　　　　更新する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0020B.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         02/02/20.
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
*ファイルインバリッド
 01  FILE-INV-AREA.
     03  JSMJIKF-INV-FLG          PIC  X(03)  VALUE  SPACE.
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
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*更新チェックワーク
 01  LINK-CHK              PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-CHK.
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
*******************
*    MOVE    "3"       TO  LINK-CHK.
     DISPLAY "起動確認更新値="  LINK-CHK UPON CONS.
*******************
     MOVE     "PROCESS-START"     TO   S-NAME.
***  ファイルのオープン
     OPEN      I-O      JSMJIKF.
***  実行制御マスタチェック
     MOVE     "S"                 TO   JIK-F01.
     READ    JSMJIKF
             INVALID
               MOVE    "INV"      TO   JSMJIKF-INV-FLG
             NOT  INVALID
               MOVE    SPACE      TO   JSMJIKF-INV-FLG
     END-READ.
***  更新判断
     IF        JSMJIKF-INV-FLG  =  "INV"
               MOVE  SPACE        TO   JIK-REC
               INITIALIZE              JIK-REC
               MOVE  "S"          TO   JIK-F01
               MOVE  NC"自動受信" TO   JIK-F02
               IF    LINK-CHK  =  "0"  OR  "1"
                     IF  LINK-CHK = "0"
                         MOVE ZERO TO  JIK-F03
                     ELSE
                         MOVE 1    TO  JIK-F03
                     END-IF
               ELSE
                     MOVE     1    TO  JIK-F03
               END-IF
               WRITE JIK-REC
     ELSE
               IF    LINK-CHK  =  "0"  OR  "1"
                     IF  LINK-CHK = "0"
                         MOVE ZERO TO  JIK-F03
                     ELSE
                         MOVE 1    TO  JIK-F03
                     END-IF
               ELSE
                     MOVE     1    TO  JIK-F03
               END-IF
               REWRITE JIK-REC
     END-IF.
***  ファイルのクローズ
     CLOSE     JSMJIKF.
***  プログラム終了
     STOP  RUN.
 PROCESS-EXD.
     EXIT.
*****************<<  NVS0020B   END PROGRAM  >>******************
