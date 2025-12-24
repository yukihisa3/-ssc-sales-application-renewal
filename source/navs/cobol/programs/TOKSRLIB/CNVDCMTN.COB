****************************************************************

*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　　*
*    サブシステム　　：　ＤＣＭ店舗名一括変更　　　　　　　　　*
*    モジュール名　　：　ＤＣＭ店舗名一括変更　　　　　　　　　*
*    作成日　　　　　：　2022/08/15                            *
*    作成者　　　　　：　NAV TAKAHASHI                         *
*    処理概要　　　　：　店舗名変更Ｆを読み、店舗マスタを索引し*
*                        店舗名の一括変更を行なう。　　　　　　*
*    更新日　　　　　：　                                      *
*    更新者　　　　　：　                                      *
*    更新概要　　　　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            CNVDCMTN.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/08/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＤＣＭ店舗名変更ファイル
     SELECT   DCMTENL1  ASSIGN    TO        DA-01-VI-DCMTENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DCM-F01
                                            DCM-F02
                        FILE  STATUS   IS   DCM-STATUS.
*店舗マスタ
     SELECT   TENMS1  ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*バックアップ店舗データ
     SELECT   BKDCMTEN  ASSIGN    TO        DA-01-S-BKDCMTEN
                        FILE      STATUS    BKD-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＤＣＭ店舗名変更ファイル
******************************************************************
 FD  DCMTENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     DCMTENL1  OF        XFDLIB
              JOINING   DCM  AS   PREFIX.
*
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
******************************************************************
*    バックアップ店舗データ
******************************************************************
 FD  BKDCMTEN.
     COPY     BKDCMTEN  OF        XFDLIB
              JOINING   BKD       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SKP-CNT                 PIC  9(08)     VALUE  ZERO.
 01  RWT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  BKD-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  FLG-AREA.
     03  TENMS1-INV-FLG    PIC  X(03)     VALUE SPACE.
*
 01  WK-ST.
     03  DCM-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  BKD-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "CNVDCMTN".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CNVDCMTN".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CNVDCMTN".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMTENL1.
     MOVE      "DCMTENL1"   TO   AB-FILE.
     MOVE      DCM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BKDCMTEN.
     MOVE      "BKDCMTEN" TO   AB-FILE.
     MOVE      BKD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DCMTENL1.
     OPEN     I-O       TENMS1.
     OPEN     EXTEND    BKDCMTEN.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-TIME       TO   SYS-TIME2.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     PERFORM  DCMTENL1-READ-SEC.
     IF       END-FLG  =  "END"
              DISPLAY NC"＃対象データがありません＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*店舗マスタ索引　　
     PERFORM  HTENMS-READ-SEC.
     IF  TENMS1-INV-FLG  =  "INV"
         ADD  1                   TO   SKP-CNT
         GO                       TO   MAIN-99
     END-IF.
*バクアップ
     MOVE     SPACE               TO   BKD-REC.
     INITIALIZE                        BKD-REC.
     MOVE     TEN-REC             TO   BKD-REC.
     WRITE    BKD-REC.
     ADD      1                   TO   BKD-CNT.
*店舗名更新
     MOVE     DCM-F03             TO   TEN-F02.
     MOVE     DCM-F04             TO   TEN-F03.
     MOVE     DCM-F05             TO   TEN-F04.
     MOVE     DCM-F06             TO   TEN-F55.
     MOVE     SYS-DATEW           TO   TEN-F99.
     REWRITE  TEN-REC.
     ADD      1                   TO   RWT-CNT.
*
 MAIN-99.
*
     PERFORM  DCMTENL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取消データ　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DCMTENL1-READ-SEC       SECTION.
*
     MOVE     "DCMTENL1-READ-SEC"  TO      S-NAME.
*
*取消データ　　　　　　　　
     READ     DCMTENL1
         AT END
              MOVE   "END"      TO    END-FLG
              GO                TO    DCMTENL1-READ-EXIT
         NOT AT END
              ADD      1        TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3)  =  "000"  OR  "500"
          DISPLAY "#RD-CNT = " RD-CNT " #"  UPON  CONS
     END-IF.
*
 DCMTENL1-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ読込　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HTENMS-READ-SEC         SECTION.
*
     MOVE     "HTENMS-READ-SEC"    TO      S-NAME.
*
     MOVE      DCM-F01             TO      TEN-F52.
     MOVE      DCM-F02             TO      TEN-F011.
     READ  TENMS1
           INVALID      MOVE "INV" TO      TENMS1-INV-FLG
           NOT  INVALID MOVE SPACE TO      TENMS1-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "RD-CNT  = " RD-CNT   UPON CONS.
     DISPLAY "SKP-CNT = " SKP-CNT  UPON CONS.
     DISPLAY "RWT-CNT   " RWT-CNT  UPON CONS.
     DISPLAY "BKD-CNT   " BKD-CNT  UPON CONS.
*
     CLOSE     DCMTENL1  TENMS1  BKDCMTEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
