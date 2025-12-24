# JISBKFLG

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/JISBKFLG.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　実績管理システム　　　　　　　　　*
*    モジュール名　　　　：　実績Ｆバックアップ判定処理        *
*    作成日／更新日　　　：　2011/11/18                        *
*    作成者／更新者　　　：　T.MIURA                           *
*    処理概要　　　　　　：　条件Ｆの実績データ退避年を参照    *
*                            更新する。                        *
****************************************************************
******************************************************************
*             IDENTIFICATION      DIVISION                       *
******************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            JISBKFLG.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/11/18.
*
******************************************************************
*                                                                *
*             ENVIRONMENT         DIVISION                       *
*                                                                *
******************************************************************
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****************************************************************
*                                                              *
*                DATA            DIVISION                      *
*                                                              *
****************************************************************
 DATA                            DIVISION.
*--------------------------------------------------------------*
*                FILE            SECTION                       *
*--------------------------------------------------------------*
 FILE                            SECTION.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****************************************************************
 WORKING-STORAGE                 SECTION.
****************************************************************
*****ｽﾃｰﾀｽ
 01  STATUS-AREA.
     03  CON-ST         PIC   X(02)    VALUE   SPACE.
     03  JYO-STATUS     PIC   X(02).
**** ﾜｰｸ
 01  INV-FLG            PIC   X(01)    VALUE   SPACE.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE.
         05  SYS-YYYY             PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "JISBKFLG".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*---------------------------------------------------------------
 LINKAGE               SECTION.
 01  LINK-PARA.
     03  LINK          PIC  X(02).
*---------------------------------------------------------------
****************************************************************
*                                                              *
*             PROCEDURE          DIVISION                      *
*                                                              *
****************************************************************
 PROCEDURE                       DIVISION
                                 USING        LINK-PARA.
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
*    0.0               基本モジュール                          *
****************************************************************
 PROGRAM-START          SECTION.
*
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC.
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 PROGRAM-START-EXIT.
     EXIT.
****************************************************************
*    1.0               初期処理                                *
****************************************************************
 INIT-SEC               SECTION.
* ファイルオープン
     MOVE    SPACE      TO        LINK.
     OPEN     I-O       HJYOKEN.
*前回退避データ年度取得
     MOVE    99          TO  JYO-F01.
     MOVE   "JISNEN"     TO  JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY "## HJYOKEN INVALID KEY = " JYO-F01 JYO-F02
                      " ##"  UPON CONS
             STOP  RUN
     END-READ.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
 INIT-EXIT.
     EXIT.
****************************************************************
*    2.0     主　処　理                                        *
****************************************************************
 MAIN-SEC               SECTION.
     IF  SYS-YYYY  >    JYO-F04
         PERFORM        REWRITE-SEC
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*    3.0     終　了　処　理                                    *
****************************************************************
 END-SEC                SECTION.
* ファイルクローズ
     CLOSE              HJYOKEN.
*
 END-EXIT.
     EXIT.
**************************************************************
*              ＲＥＷＲＩＴＥ  処理                          *
**************************************************************
 REWRITE-SEC            SECTION.
* ファイルＲＥＷＲＩＴＥ
     MOVE       SYS-YYYY     TO   JYO-F04.
     REWRITE    JYO-REC.
     MOVE       "ON"              TO   LINK.
*
 REWRITE-SEC-EXIT.
     EXIT.
 END          PROGRAM   JISBKFLG.

```
