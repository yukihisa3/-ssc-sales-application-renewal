****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム改善　　　　　　　*
*    モジュール名　　　　：　実績連携データ抽出（Ｋ→ＰＣ）　　*
*    作成日／更新日　　　：　09/10/13   09/11/25               *
*    作成者／更新者　　　：　ＮＡＶ　大野　　　　　　　　　　　*
*    処理概要　　　　　　：　実績連携Ｆ（累積用）をキー３で順で*
*                            読込む。ＰＣ側データ作成区分＝”空*
*                            白”のデータが、実績連携Ｆ（オフコ*
*                            ン→ＰＣ）に重複しなければ更新する*
*                      　：　　更新後、実績系連携Ｆ（オフコン累*
*                            積用）のＰＣ側データ作成区分、作成*
*                            日付、作成時刻を更新する。　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0231B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/10/13.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 実績連携Ｆ（オフコン累積用） >>--*
     SELECT   PCJISSF   ASSIGN         DA-01-VI-PCJISSL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SSF-F95
                                       SSF-F01   SSF-F02
                                       SSF-F03   SSF-F04
                                       SSF-F23   SSF-F22
                                       SSF-F06   SSF-F11
                                       SSF-F07   SSF-F08
                        STATUS         SSF-ST.
*----<< 実績連携Ｆ（オフコン→ＰＣ） >>--*
     SELECT   PCJISWL1  ASSIGN         DA-01-VI-PCJISWL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SWL-F01   SWL-F02
                                       SWL-F03   SWL-F04
                                       SWL-F23   SWL-F22
                                       SWL-F06   SWL-F11
                                       SWL-F07   SWL-F08
                        STATUS         SWL-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 実績連携Ｆ（オフコン累積用） >>--*
 FD  PCJISSF            LABEL     RECORD   IS   STANDARD.
     COPY     PCJISSL3  OF        XFDLIB
              JOINING   SSF       PREFIX.
*----<< 実績連携Ｆ（オフコン→ＰＣ） >>--*
 FD  PCJISWL1           LABEL     RECORD   IS   STANDARD.
     COPY     PCJISWL1  OF        XFDLIB
              JOINING   SWL       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(09).
     03  OUT-CNT        PIC  9(09).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  WK-STATUS.
     03  SSF-ST         PIC  X(02).
     03  SWL-ST         PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  WK-SWL-FIL         PIC  X(09).
 01  WK-SWL-FIL-R       REDEFINES  WK-SWL-FIL.
     03  WK-SWL-FIL-D   PIC  9(09).
*
 01  WK-SAK-DATE.
     03  SAK-YYYY       PIC  9(04).
     03  SAK-MM         PIC  9(02).
     03  SAK-DD         PIC  9(02).
 01  WK-SAK-TIME        PIC  9(06).
*
 01  LINK-AREA.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
 LINKAGE                SECTION.
 01  PARA-SCNT          PIC  9(09).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-SCNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 実績連携Ｆ（オフコン累積用） >>--*
 PCJISSF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCJISSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJS0231B PCJISSL3 ERROR " SSF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    PCJISSF  PCJISWL1.
     STOP     RUN.
*----<< 実績連携Ｆ（オフコン→ＰＣ） >>--*
 PCJISWL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCJISWL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJS0231B PCJISWL1 ERROR " SWL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    PCJISSF  PCJISWL1.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      初期処理                                    *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJS0231B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS       UPON CONS.
*        システム日付８桁変換
     MOVE    "3"             TO       LINK-IN-KBN.
     MOVE     SYS-DATE       TO       LINK-IN-YMD6.
     CALL    "SKYDTCKB"      USING    LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO       WK-SAK-DATE.
     MOVE     SYS-TIME(1:6)  TO       WK-SAK-TIME.
*
     OPEN     I-O       PCJISSF.
     OPEN     I-O       PCJISWL1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     PERFORM   900-SSF-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理                                  *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE      SSF-F01       TO     SWL-F01.
     MOVE      SSF-F02       TO     SWL-F02.
     MOVE      SSF-F03       TO     SWL-F03.
     MOVE      SSF-F04       TO     SWL-F04.
     MOVE      SSF-F23       TO     SWL-F23.
     MOVE      SSF-F22       TO     SWL-F22.
     MOVE      SSF-F06       TO     SWL-F06.
     MOVE      SSF-F11       TO     SWL-F11.
     MOVE      SSF-F07       TO     SWL-F07.
     MOVE      SSF-F08       TO     SWL-F08.
*
     READ      PCJISWL1
          INVALID
               PERFORM       300-WRITE-RTN
     END-READ.
*
     PERFORM  900-SSF-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      実績連携Ｆ更新処理                          *
*--------------------------------------------------------------*
 300-WRITE-RTN           SECTION.
*
     MOVE      SPACE         TO     SWL-REC.
     INITIALIZE                     SWL-REC.
     MOVE      SSF-REC       TO     SWL-REC.
*
     IF        SSF-F05  NOT =  "I3"
               IF  SSF-F04  =  "31"  OR  "32"
                   CONTINUE
               ELSE
                   MOVE   SSF-FIL(1:9)  TO  WK-SWL-FIL
                   MOVE   WK-SWL-FIL-D  TO  SWL-F07
               END-IF
     END-IF.
*
     MOVE     "1"            TO     SWL-F95.
     WRITE     SWL-REC.
     ADD       1             TO     OUT-CNT.
*
     MOVE     "1"            TO     SSF-F95.
     MOVE      WK-SAK-DATE   TO     SSF-F96.
     MOVE      WK-SAK-TIME   TO     SSF-F97.
     REWRITE   SSF-REC.
*
 300-WRITE-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    PCJISSF  PCJISWL1.
*出力件数パラメタセット
     MOVE     OUT-CNT        TO      PARA-SCNT.
*
     DISPLAY  "+++ ﾆｭｳﾘｮｸｹﾝｽｳ =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ 更新件数 = " OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJS0231B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS         UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    実績連携Ｆ（オフコン累積用）　 READ          *
*--------------------------------------------------------------*
 900-SSF-READ           SECTION.
     READ     PCJISSF   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-SSF-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
*    件数表示
     IF       SSF-F95    NOT =  SPACE
              MOVE      1              TO   END-FLG
              GO   TO   900-SSF-READ-EXIT
     END-IF.
*
 900-SSF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
