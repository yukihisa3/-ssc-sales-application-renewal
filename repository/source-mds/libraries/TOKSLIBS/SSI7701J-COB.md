# SSI7701J

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI7701J.COB`

## ソースコード

```cobol
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
 PROGRAM-ID.            SSI7701J.
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
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  IX             PIC  9(01).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JSS-ST             PIC  X(02).
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
 LINKAGE                SECTION.
 01  LINK-CNT           PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-CNT.
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
     DISPLAY  "### SSI7701J SANDYJSS    ERROR " JSS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKFS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI7701J SITGKFS ERROR " SHI-ST " "
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
     DISPLAY  "*** SSI7701J START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SANDYJSS.
     OPEN     OUTPUT    SITGKFS.
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
                 WRITE  SHI-REC
                 ADD    1             TO     OUT-CNT
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
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SANDYJSS.
     CLOSE    SITGKFS.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     MOVE     OUT-CNT        TO   LINK-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI7701J END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
