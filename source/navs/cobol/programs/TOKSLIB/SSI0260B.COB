****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　ホーマック関東オンライン　　　　　*
*　　モジュール名　　　　：　支払合計ファイル作成　　　　　　　*
*　　作成日／更新日　　　：　00/08/10                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　支払データより支払合計ファイルを　*
*　　　　　　　　　　　　：　作成する                          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI0260B.
 AUTHOR.                Y.YOSHIDA.
 DATE-WRITTEN.          00/08/10.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払明細ファイル >>--*
     SELECT   HOMCF     ASSIGN         DA-01-S-HOMCF
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< 支払合計ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  HOMCF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL1-REC.
         05  ONL1-A1              PIC  X(01).
         05  ONL1-A2              PIC  9(04).
         05  ONL1-A3              PIC  9(06).
         05  ONL1-A4              PIC  9(10).
         05  ONL1-A5              PIC  9(06).
         05  ONL1-A6              PIC  X(101).
     03  ONL2-REC  REDEFINES      ONL1-REC.
         05  ONL2-A01             PIC  X(01).
         05  ONL2-A02             PIC  9(04).
         05  ONL2-A03             PIC  9(06).
         05  ONL2-A04             PIC  9(05).
         05  ONL2-A06             PIC  9(06).
         05  ONL2-A06             PIC  9(05).
         05  ONL2-A07             PIC  9(02).
         05  ONL2-A08             PIC  9(11).
         05  ONL2-A09             PIC  9(10).
         05  ONL2-A10             PIC  9(02).
         05  ONL2-A11             PIC  9(02).
         05  ONL2-A12             PIC  9(04).
         05  ONL2-A13             PIC  9(07).
         05  ONL2-A14             PIC  9(06).
         05  ONL2-A15             PIC  9(10).
         05  ONL2-A16             PIC  9(01).
         05  ONL2-A17             PIC  9(01).
         05  ONL2-A18             PIC  9(10).
         05  ONL2-A19             PIC  9(01).
         05  ONL2-A20             PIC  9(06).
         05  ONL2-A21             PIC  9(06).
         05  ONL2-A22             PIC  9(10).
         05  ONL2-A23             PIC  X(12).
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        SITGKFF     OF      XFDLIB
                 JOINING     SHI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  CVCS-ST            PIC  X(02).
 01  SHI-ST             PIC  X(02).
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
*----<< 集信データ >>--*
 CVCSK-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOMCF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0260B HOMCF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0260B HSHIGKF ERROR " SHI-ST " "
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
     DISPLAY  "*** SSI0260B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOMCF.
     OPEN     OUTPUT    HSHIGKF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
 INIT-010.
*
     READ     HOMCF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
              IF   ONL1-A1   =   "H"
                   MOVE      ONL1-A4   TO   WK-GOKEI
                   GO   TO   INIT-010
              END-IF
              IF   ONL1-A1   =   "T"
                   GO   TO   INIT-010
              END-IF
     END-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     MOVE     SPACE          TO   SHI-REC.
     INITIALIZE                   SHI-REC.
     MOVE     ONL-REC        TO   SHI-REC.
*****DISPLAY "ONL2-A15 = " ONL2-A15 UPON CONS.
     MOVE     WK-GOKEI       TO   SHI-F23.
     WRITE    SHI-REC.
     ADD      1              TO   OUT-CNT.
*
 MAIN-01.
*
     READ     HOMCF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
              IF   ONL1-A1   =   "H"
                   MOVE      ONL1-A4   TO   WK-GOKEI
                   GO   TO   MAIN-01
              END-IF
              IF   ONL1-A1   =   "T"
                   GO   TO   MAIN-01
              END-IF
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    HOMCF.
     CLOSE    HSHIGKF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0260B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
