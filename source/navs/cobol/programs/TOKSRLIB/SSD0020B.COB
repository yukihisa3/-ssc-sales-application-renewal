****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　システム始動時処理制御　　　　　　*
*    業務名　　　　　　　：　システム始動時処理制御            *
*    モジュール名　　　　：　初期起動ＦＬＧ取得　　            *
*    作成日／更新日　　　：　2020/08/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　条件Ｆを読み、初期起動ＦＬＧを取得*
*                            する。　　　                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSD0020B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/08/28.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01  JYO-F02
                       FILE      STATUS    JYO-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 条件ファイル　　                                   *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JYO-ST                   PIC  X(02).
*ファイルインバリッド
 01  FILE-INV-AREA.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー　".
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
*    パラメタ保管
 01  WK-LINK-KBN                  PIC  X(01)  VALUE  SPACE.
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*更新チェックワーク
 01  LINK-KBN              PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-KBN.
**************************************************************
 DECLARATIVES.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "HJYOKEN" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***  ファイルのオープン
     OPEN      INPUT    HJYOKEN.
***  条件ファイル読込
     MOVE     99                  TO   JYO-F01.
     MOVE     "SIDOUJOB"          TO   JYO-F02.
     READ    HJYOKEN
             INVALID
               MOVE    "INV"      TO   HJYOKEN-INV-FLG
             NOT  INVALID
               MOVE    SPACE      TO   HJYOKEN-INV-FLG
     END-READ.
***  更新判断
     IF        HJYOKEN-INV-FLG  =  "INV"
               MOVE  "2"          TO   WK-LINK-KBN
     ELSE
               IF  JYO-F15  =  "ON "
                   MOVE  "1"      TO   WK-LINK-KBN
               END-IF
               IF  JYO-F15  =  "OFF"
                   MOVE  "2"      TO   WK-LINK-KBN
               END-IF
     END-IF.
*
     MOVE      WK-LINK-KBN        TO   LINK-KBN.
     DISPLAY NC"＃初期起動ＦＬＧ" " = " WK-LINK-KBN UPON CONS.
*
***  ファイルのクローズ
     CLOSE     HJYOKEN.
***  プログラム終了
     STOP  RUN.
 PROCESS-EXD.
     EXIT.
*****************<<  SSD0020B   END PROGRAM  >>******************
