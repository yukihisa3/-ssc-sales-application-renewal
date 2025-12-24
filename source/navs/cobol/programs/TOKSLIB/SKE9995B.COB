****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　累積梱包データコンバート　　　　　*
*　　作成日／更新日　　　：　2002/08/22                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　累積梱包ラベルデータコンバート    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE9995B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/11/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 旧累積出荷ラベルＦ >>--*
     SELECT   RUIKONSF  ASSIGN         DA-01-S-RUIKONSF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RUS-ST.
*----<< 累積出荷ラベル明細Ｆ >>--*
     SELECT   RUIKONF   ASSIGN         DA-01-VI-RUIKONF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F08 RUI-F02 RUI-F04
                        STATUS         RUI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 旧累積出荷ラベルＦ >>--*
 FD  RUIKONSF           LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       40   RECORDS.
     COPY        RUIKONSF    OF      XFDLIB
                 JOINING     RUS     PREFIX.
*----<< 累積出荷ラベル明細Ｆ >>--*
 FD  RUIKONF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RUIKONF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)  VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUS-ST             PIC  X(02).
 01  RUI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 旧累積出荷ラベルＦ >>--*
 RUS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUIKONSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE9995B RUIKONSF   ERROR " RUS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 累積出荷ラベルＦ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUIKONF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE9995B RUIKONF ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE9995B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RUIKONSF.
     OPEN     I-O       RUIKONF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  210-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*キー項目転送
     MOVE     RUS-F04(8:4)   TO   RUI-F08.
     MOVE     RUS-F02        TO   RUI-F02.
     MOVE     RUS-F04        TO   RUI-F04.
*    累積ファイル読む
     PERFORM  220-READ-SEC.
*
     EVALUATE CHK-FLG
         WHEN "O"
              CONTINUE
         WHEN "E"
              MOVE SPACE          TO   RUI-REC
              INITIALIZE               RUI-REC
              MOVE RUS-F01        TO   RUI-F01
              MOVE RUS-F02        TO   RUI-F02
              MOVE RUS-F03        TO   RUI-F03
              MOVE RUS-F04        TO   RUI-F04
              MOVE RUS-F05        TO   RUI-F05
              MOVE RUS-F06        TO   RUI-F06
              MOVE RUS-F07        TO   RUI-F07
              MOVE RUS-F08        TO   RUI-F08
              WRITE     RUI-REC
              ADD  1              TO   OUT-CNT
     END-EVALUATE.
*
 MAIN-01.
*
     PERFORM  210-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RUIKONSF.
     CLOSE    RUIKONF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE9995B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 210-READ-SEC           SECTION.
*
     READ     RUIKONSF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 210-READ-EXIT.
     EXIT.
****************************************************************
*　　　　アウトプットデータ読む　　　　　　　　　　　　　　　　*
****************************************************************
 220-READ-SEC           SECTION.
*
     READ     RUIKONF
        INVALID
              MOVE     "E"   TO   CHK-FLG
        NOT INVALID
              MOVE     "O"   TO   CHK-FLG
     END-READ.
*
 220-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
