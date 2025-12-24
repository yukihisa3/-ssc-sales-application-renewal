****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　サンデー支払照合　　　　　　　　　*
*　　モジュール名　　　　：　サンデー支払データ変換　　　　　　*
*　　作成日／更新日　　　：　08/10/09                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　受信した支払データを社内支払データ*
*　　　　　　　　　　　　：　に変換する。　　　　              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI7701B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/10/09.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払明細ファイル >>--*
     SELECT   SANDYJSS  ASSIGN         DA-01-S-SANDYJSS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         JSS-ST.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFS   ASSIGN         DA-01-S-SITGKFS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFJ   ASSIGN         DA-01-S-SITGKFJ
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHJ-ST.
*----<< 店舗マスタ　　　 >>--*
     SELECT   HTENMS    ASSIGN    TO   DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  SANDYJSS           LABEL     RECORD    IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        SANDYJSS    OF      XFDLIB
                 JOINING     JSS     PREFIX.
*2001/01/09 ﾚｲｱｳﾄ変更 END   *
*----<< 支払合計ファイル >>--*
 FD  SITGKFS            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       63   RECORDS.
     COPY        SITGKFE     OF      XFDLIB
                 JOINING     SHI     PREFIX.
*
*----<< 支払合計ファイル >>--*
 FD  SITGKFJ            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       63   RECORDS.
     COPY        SITGKFJ     OF      XFDLIB
                 JOINING     SHJ     PREFIX.
*
*----<< 店舗マスタ　　　 >>--*
 FD  HTENMS.
     COPY        HTENMS      OF      XFDLIB
                 JOINING     TEN     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  IX             PIC  9(01).
     03  OUT-CNTS       PIC  9(06).
     03  OUT-CNTJ       PIC  9(06).
*
 01  HTENMS-INV-FLG     PIC  X(03)  VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JSS-ST             PIC  X(02).
 01  SHI-ST             PIC  X(02).
 01  SHJ-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
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
 LINKAGE                SECTION.
 01  LINK-CNT           PIC  9(07).
 01  LINK-CNT2          PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-CNT  LINK-CNT2.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 集信データ >>--*
 CVCSK-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SANDYJSS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSI7701B SANDYJSS    ERROR " JSS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKFS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSI7701B SITGKFS ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ　　　 >>--*
 TEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSI7701B TENMS1  ERROR " TEN-ST " "
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
     DISPLAY  "*** SSI7701B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SANDYJSS  HTENMS.
     OPEN     OUTPUT    SITGKFS   SITGKFJ.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  SANDYJSS-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SANDYJSS-READ-SEC      SECTION.
*
     READ     SANDYJSS
        AT    END
              MOVE      1    TO   END-FLG
              GO             TO   SANDYJSS-READ-EXIT
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
     IF       IN-CNT(4:3) = "000" OR "500"
              DISPLAY "IN-CNT = " IN-CNT  UPON CONS
     END-IF.
*
     IF       JSS-F02  NOT  NUMERIC
              GO             TO   SANDYJSS-READ-SEC
     END-IF.
*
 SANDYJSS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
             IF  JSS-F041(IX)  NUMERIC
                 MOVE   SPACE         TO     SHI-REC
                 INITIALIZE                  SHI-REC
                 MOVE   ZERO          TO     SHI-F01
                 MOVE   JSS-F02       TO     SHI-F02
                 MOVE   ZERO          TO     SHI-F03
                 MOVE   JSS-F045(IX)  TO     SHI-F04
                 MOVE   JSS-F043(IX)  TO     SHI-F05
                 MOVE   JSS-F046(IX)  TO     SHI-F06
                 IF     JSS-F043(IX) = 2
                        IF JSS-F047(IX) = "-"
                           COMPUTE SHI-F06 = JSS-F046(IX)
                        ELSE
                            COMPUTE SHI-F06 = JSS-F046(IX) * -1
                        END-IF
                 ELSE
                        IF JSS-F047(IX) = "-"
                           COMPUTE SHI-F06 = JSS-F046(IX) * -1
                        END-IF
                 END-IF
*****************IF JSS-F047(IX) = "-"
*                   COMPUTE SHI-F06 = JSS-F046(IX) * -1
*                END-IF
*****************
                 MOVE   JSS-F041(IX)  TO     SHI-F07
                 MOVE   ZERO          TO     SHI-F08
                 MOVE   JSS-F044(IX)  TO     SHI-F09
                 MOVE   30402         TO     TEN-F52
                 MOVE   JSS-F041(IX)  TO     TEN-F011
                 PERFORM HTENMS-READ-SEC
*****************店舗マスタが存在しない場合は、ＪＯＹ側に出力
                 IF  HTENMS-INV-FLG = SPACE
                     WRITE  SHI-REC
                     ADD    1             TO     OUT-CNTS
                 ELSE
                     MOVE   SPACE         TO     SHJ-REC
                     INITIALIZE                  SHJ-REC
                     MOVE   SHI-REC       TO     SHJ-REC
                     MOVE   304021        TO     SHJ-F02
                     WRITE  SHJ-REC
                     ADD    1             TO     OUT-CNTJ
                 END-IF
             END-IF
     END-PERFORM.
*
 MAIN-01.
*
     PERFORM  SANDYJSS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　店舗マスタ索引                                        *
****************************************************************
 HTENMS-READ-SEC        SECTION.
*
     READ  HTENMS
           INVALID     MOVE "INV"     TO   HTENMS-INV-FLG
           NOT INVALID MOVE SPACE     TO   HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SANDYJSS.
     CLOSE    SITGKFS.
     CLOSE    SITGKFJ.
     CLOSE    HTENMS.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT SDY =" OUT-CNTS " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT JOY =" OUT-CNTJ " +++" UPON CONS.
     MOVE     OUT-CNTS       TO   LINK-CNT.
     MOVE     OUT-CNTJ       TO   LINK-CNT2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI7701B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
