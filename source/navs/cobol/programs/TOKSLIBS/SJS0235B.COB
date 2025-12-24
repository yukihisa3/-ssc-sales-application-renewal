****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム改善　　　　　　　*
*    モジュール名　　　　：　送信ファイル件数カウント　　　　　*
*    作成日／更新日　　　：　10/01/18                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　送信用ファイルを読み、件数をカウン*
*                            トして、パラメタに渡す。　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0235B.
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
*----<< 実績連携Ｆ（オフコン→ＰＣ） >>--*
     SELECT   PCJISWL1  ASSIGN         DA-01-VI-PCJISWL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
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
*----<< 実績連携Ｆ（オフコン→ＰＣ） >>--*
 PCJISWL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCJISWL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SJS0235B PCJISWL1 ERROR " SWL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    PCJISWL1.
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
     DISPLAY  "*** SJS0235B START *** "
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
     OPEN     I-O       PCJISWL1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     PERFORM   900-SWL-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理                                  *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     PERFORM  900-SWL-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    PCJISWL1.
*出力件数パラメタセット
     MOVE     IN-CNT         TO      PARA-SCNT.
*
     DISPLAY  "+++ ﾆｭｳﾘｮｸｹﾝｽｳ =" IN-CNT  " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJS0235B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS         UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    実績連携Ｆ（オフコン累積用）　 READ          *
*--------------------------------------------------------------*
 900-SWL-READ           SECTION.
*
     READ     PCJISWL1  AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-SWL-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
*
 900-SWL-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
