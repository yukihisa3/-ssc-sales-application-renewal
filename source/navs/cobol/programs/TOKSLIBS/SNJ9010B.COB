****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　自動受信起動／停止更新            *
*    作成日／更新日　　　：　2010/09/02                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタより受取り、起動確認区分を*
*                            更新する。                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ9010B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         10/09/02.
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
 JIK-ERR                   SECTION.
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
     OPEN      I-O      JSMJIKF.
***  実行制御マスタチェック
     MOVE     "J"                 TO   JIK-F01.
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
               MOVE  "J"          TO   JIK-F01
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
*****************<<  SNJ9010B   END PROGRAM  >>******************
