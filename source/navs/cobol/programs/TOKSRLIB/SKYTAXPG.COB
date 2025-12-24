****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　消費税軽減税率対応　　　　　　　　*
*    業務名　　　　　　　：　消費税軽減税率対応　　　　        *
*    モジュール名　　　　：　消費税率取得サブ　　　　　　　　　*
*    作成日／作成者　　　：　2019/09/18 NAV TAKAHASHI          *
*    処理概要　　　　　　：　パラメタを受け取り、条件Ｆより　　*
*                          　消費税率を取得する。　　　　　　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2019/09/18 高橋_　　新規作成
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKYTAXPG.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/09/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        FILE STATUS    JYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  ZERO.
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JYO-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-IN-ZEIKBN        PIC X(01).
 01  LINK-IN-DATE          PIC 9(08).
 01  LINK-OUT-ERR          PIC X(01).
 01  LINK-OUT-ZEI          PIC 9(05).
 01  LINK-OUT-DZEI         PIC 9(05).
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-ZEIKBN
                                  LINK-IN-DATE
                                  LINK-OUT-ERR
                                  LINK-OUT-ZEI
                                  LINK-OUT-DZEI.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKYTAXPG JYOKEN1  ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
     OPEN     INPUT     HJYOKEN.
*初期化
     MOVE     "0"            TO    LINK-OUT-ERR.
     MOVE     ZERO           TO    LINK-OUT-ZEI  LINK-OUT-DZEI.
*
*****DISPLAY "LINK-IN-ZEIKBN = " LINK-IN-ZEIKBN UPON CONS.
*****DISPLAY "LINK-IN-DATE   = " LINK-IN-DATE   UPON CONS.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*代表消費税取得
     MOVE     99                  TO   JYO-F01.
     MOVE     "ZEI"               TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF       HJYOKEN-INV-FLG  =  "INV"
              MOVE  "9"           TO   LINK-OUT-ERR
              GO                  TO   200-MAIN-RTN-EXIT
     ELSE
              IF   JYO-F06  <=  LINK-IN-DATE
                   COMPUTE  LINK-OUT-DZEI =  JYO-F08 * 10000
              ELSE
                   COMPUTE  LINK-OUT-DZEI =  JYO-F05 * 10000
              END-IF
     END-IF.
*税区分判定
*  税区分＝０の時
     IF       LINK-IN-ZEIKBN  =  "0"
              MOVE  LINK-OUT-DZEI TO   LINK-OUT-ZEI
              GO                  TO   200-MAIN-RTN-EXIT
     END-IF.
*　税区分＝０以外の時
     MOVE     50                  TO   JYO-F01.
     MOVE     LINK-IN-ZEIKBN      TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF       HJYOKEN-INV-FLG  =  "INV"
              MOVE  "1"           TO   LINK-OUT-ERR
              GO                  TO   200-MAIN-RTN-EXIT
     ELSE
              COMPUTE LINK-OUT-ZEI =  JYO-F06 * 10000
     END-IF.

*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    HJYOKEN.
*
*****DISPLAY "LINK-OUT-ZEI   = " LINK-OUT-ZEI   UPON CONS.
*****DISPLAY "LINK-OUT-DZEI  = " LINK-OUT-DZEI  UPON CONS.
*****DISPLAY "LINK-OUT-ERR   = " LINK-OUT-ERR   UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイル　　　読込
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
     MOVE    "HJYOKEN-READ-SEC"   TO   S-NAME.
*
     READ  HJYOKEN      INVALID
                        MOVE "INV" TO  HJYOKEN-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
