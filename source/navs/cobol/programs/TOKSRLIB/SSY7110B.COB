****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ジョイフル本田　特別処理　　　　　*
*    モジュール名　　　　：　原価単価再セット処理　　　　　　　*
*    作成日／更新日　　　：　2019/12/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　バッチ_を受け取り、売上伝票Ｆに　*
*                            スタートを掛け、原価単価＝０の場　*
*                            合、変換ＴＢＬより単価をセットする*
*    作成日／更新日　　　：　                                  *
*    作成者／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7110B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/12/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                        STATUS         DEN-ST1.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-ST1.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)  VALUE  ZERO.
     03  INV-FLG        PIC  9(01)  VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06)  VALUE  ZERO.
     03  TAI-CNT        PIC  9(06)  VALUE  ZERO.
     03  OUT-CNT        PIC  9(06)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST.
     03  DEN-ST1        PIC  X(02).
     03  DEN-ST2        PIC  X(04).
 01  TBL-ST.
     03  TBL-ST1        PIC  X(02).
     03  TBL-ST2        PIC  X(04).
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
*金額
 01  WK-YEN             PIC S9(11)  VALUE  ZERO.

 LINKAGE                SECTION.
 01  PARA-HIDUKE        PIC  9(08).
 01  PARA-JIKAN         PIC  9(04).
 01  PARA-TOKCD         PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-HIDUKE
                                         PARA-JIKAN
                                         PARA-TOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY7110B SHTDENF ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTDENF  HSHOTBL.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY7110B HSHOTBL ERROR " TBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    SHTDENF  HSHOTBL.
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
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7110B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS       UPON CONS.
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     HSHOTBL.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     MOVE      SPACE         TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE      PARA-HIDUKE   TO   DEN-F46.
     MOVE      PARA-JIKAN    TO   DEN-F47.
     MOVE      PARA-TOKCD    TO   DEN-F01.
     START  SHTDENF  KEY  IS  >=  DEN-F46  DEN-F47  DEN-F01
                                  DEN-F48  DEN-F02  DEN-F04
                                  DEN-F051 DEN-F07  DEN-F112
                                  DEN-F03
            INVALID
            MOVE     1       TO   END-FLG
            GO               TO   100-INIT-RTN-EXIT
     END-START.
*
     PERFORM   900-DEN-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     INITIALIZE             TBL-REC.
     MOVE   DEN-F01    TO   TBL-F01.
     MOVE   DEN-F25    TO   TBL-F02.
     READ   HSHOTBL
       INVALID
         DISPLAY "TBL-F01 = " TBL-F01  UPON CONS
         DISPLAY "TBL-F02 = " TBL-F02  UPON CONS
         GO            TO   MAIN-010
     END-READ.
*原価単価セット
     MOVE    TBL-F05   TO   DEN-F172.
     MOVE    TBL-F05   TO   DEN-F512.
*原価金額計算
     COMPUTE WK-YEN = DEN-F15  *  DEN-F172.
     MOVE    WK-YEN    TO   DEN-F181.
     COMPUTE WK-YEN = DEN-F50  *  DEN-F512.
     MOVE    WK-YEN    TO   DEN-F522.
*更新処理
     REWRITE DEN-REC.
     ADD       1       TO   OUT-CNT.
*
 MAIN-010.
     PERFORM  900-DEN-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    SHTDENF HSHOTBL.
*
     DISPLAY  "+++ ﾆｭｳﾘｮｸｹﾝｽｳ =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ﾀｲｼｮｳ ｹﾝｽｳ =" TAI-CNT " +++" UPON CONS.
     DISPLAY  "+++ ｺｳｼﾝ  ｹﾝｽｳ =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7110B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS         UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     SHTDENF   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
*    件数表示
     IF       IN-CNT(4:3) = "000"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*指定パラメタ以上の場合は処理終了
     IF   DEN-F46  >  PARA-HIDUKE
     OR   DEN-F47  >  PARA-JIKAN
     OR   DEN-F01  >  PARA-TOKCD
          MOVE     1        TO     END-FLG
          GO                TO     900-DEN-READ-EXIT
     END-IF.
*原価単価＝０以外は対象外とする
     IF   DEN-F172  =  ZERO
     OR   DEN-F512  =  ZERO
          CONTINUE
     ELSE
          GO                TO     900-DEN-READ
     END-IF.
*
     ADD  1                 TO     TAI-CNT.
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
