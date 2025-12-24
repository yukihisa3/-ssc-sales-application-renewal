# SSI7103B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI7103B.COB`

## ソースコード

```cobol
****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　ジョイフル本田オンラインシステム　*
*　　モジュール名　　　　：　支払合計ファイル作成　　　　　　　*
*　　作成日／更新日　　　：　05/10/21                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　支払データより支払合計ファイルを　*
*　　　　　　　　　　　　：　作成する                          *
*　　作成日／更新日　　　：　17/04/17                          *
*　　作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　店舗ＣＤ＝４桁対応　　　　　　　　*
*　　　　　　　　　　　　：　　　　　　　                      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI7103B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/10/21.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払ファイル >>--*
     SELECT   JYOFUL    ASSIGN         DA-01-S-JYOFUL
                        ORGANIZATION   SEQUENTIAL
                        STATUS         JYOFUL-ST.
*----<< 支払明細ファイル >>--*
     SELECT   SITGK71   ASSIGN         DA-01-VI-SITGK711
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   SEQUENTIAL
                        RECORD  KEY    SGK-F01
                                       SGK-F02
                                       SGK-F04
                                       SGK-F05
                                       SGK-F06
                        STATUS         SGK-ST.
*----<< 支払合計ファイル >>--*
     SELECT   SITGG71   ASSIGN         DA-01-VI-SITGG711
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   SEQUENTIAL
                        RECORD  KEY    SGG-F01
                        STATUS         SGG-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  JYOFUL             BLOCK     CONTAINS 1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL1-REC.
         05  ONL1-A01             PIC  X(02).
         05  ONL1-A02             PIC  X(02).
         05  ONL1-A03             PIC  9(05).
         05  ONL1-A04             PIC  9(08).
         05  ONL1-A05             PIC  9(06).
         05  ONL1-A06             PIC  9(08).
         05  ONL1-A07             PIC  9(06).
         05  ONL1-A08             PIC  9(05).
         05  ONL1-A09             PIC  X(01).
         05  ONL1-A10             PIC  9(05).
         05  ONL1-A11             PIC  X(01).
         05  ONL1-A12             PIC  X(79).
     03  ONL2-REC  REDEFINES      ONL1-REC.
         05  ONL2-B01             PIC  X(02).
         05  ONL2-B02             PIC  X(02).
         05  ONL2-B03             PIC  9(05).
         05  ONL2-B04             PIC  9(10).
         05  ONL2-B05             PIC  X(05).
         05  ONL2-B06             PIC  X(01).
         05  ONL2-B07             PIC  X(04).
         05  ONL2-B08             PIC  X(20).
         05  ONL2-B09             PIC  X(10).
         05  ONL2-B10             PIC  9(08).
         05  ONL2-B11             PIC  9(08).
         05  ONL2-B12             PIC  9(08).
         05  ONL2-B13             PIC  X(45).
     03  ONL3-REC  REDEFINES      ONL1-REC.
         05  ONL3-C01             PIC  X(02).
         05  ONL3-C02             PIC  X(02).
         05  ONL3-C03             PIC  9(05).
         05  ONL3-C04             PIC  X(02).
         05  ONL3-C05             PIC  X(01).
         05  ONL3-C06             PIC  X(04).
         05  ONL3-C07             PIC  X(04).
         05  ONL3-C08             PIC  9(08).
         05  ONL3-C09             PIC  9(08).
         05  ONL3-C10             PIC  9(07).
         05  ONL3-C11             PIC S9(10).
         05  ONL3-C12             PIC S9(09).
         05  ONL3-C13             PIC  X(02).
         05  ONL3-C131            PIC  X(02).
         05  ONL3-C14             PIC S9(10).
         05  ONL3-C15             PIC S9(09).
         05  ONL3-C16             PIC  X(02).
         05  ONL3-C17             PIC  X(02).
         05  ONL3-C18             PIC  X(02).
         05  ONL3-C19             PIC  X(37).
     03  ONL4-REC  REDEFINES      ONL1-REC.
         05  ONL4-D01             PIC  X(02).
         05  ONL4-D02             PIC  X(02).
         05  ONL4-D03             PIC  9(05).
         05  ONL4-D04             PIC  9(06).
         05  ONL4-D05             PIC S9(10).
         05  ONL4-D06             PIC S9(09).
         05  ONL4-D07             PIC  9(06).
         05  ONL4-D08             PIC  9(06).
         05  ONL4-D09             PIC  9(06).
         05  ONL4-D10             PIC S9(10).
         05  ONL4-D11             PIC S9(09).
         05  ONL4-D12             PIC S9(10).
         05  ONL4-D13             PIC  X(47).
*----<< 支払明細ファイル >>--*
 FD  SITGK71            LABEL RECORD   IS   STANDARD.
     COPY        SITGK71     OF      XFDLIB
                 JOINING     SGK     PREFIX.
*----<< 支払合計ファイル >>--*
 FD  SITGG71            LABEL RECORD   IS   STANDARD.
     COPY        SITGG71     OF      XFDLIB
                 JOINING     SGG     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  END1-FLG       PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-SIMEBI          PIC  9(06)     VALUE  ZERO.
*#2017/04/17 NAV ST
*01  WK-TENCD           PIC  9(02)     VALUE  ZERO.
 01  WK-TENCD           PIC  9(04)     VALUE  ZERO.
*#2017/04/17 NAV ED
 01  IDX                PIC  9(02)     VALUE  1.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JYOFUL-ST          PIC  X(02).
 01  SGK-ST             PIC  X(02).
 01  SGG-ST             PIC  X(02).
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
     USE AFTER     EXCEPTION PROCEDURE      JYOFUL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI7103B JYOFUL   ERROR " JYOFUL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払明細ファイル >>--*
 SGK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGK71.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI7103B SITGK71 ERROR " SGK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SGG-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGG71.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI7103B SITGG71 ERROR " SGG-ST " "
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
     DISPLAY  "*** SSI7103B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     JYOFUL.
     OPEN     OUTPUT    SITGK71     SITGG71.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*初期読込
     READ     JYOFUL
        AT    END
              MOVE      1    TO   END-FLG
        NOT   AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*

     MOVE     ZERO           TO   END1-FLG.
     MOVE     1              TO   IDX.
*データ種区分判定
     EVALUATE ONL1-A01
         WHEN "05"
              PERFORM        TENSO-SEC   UNTIL END1-FLG = 1
         WHEN OTHER
**************PERFORM        TENSO-SEC   UNTIL END1-FLG = 1
              CONTINUE
     END-EVALUATE.
     MOVE          1         TO          END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　支払データ転送処理　　　　　　　　　　　　*
****************************************************************
 TENSO-SEC        SECTION.

*    締日格納
     IF      ONL2-B02       =     "H1"
             MOVE     ONL2-B10(3:6)      TO      WK-SIMEBI
*************#2017/04/17 NAV ST
*************MOVE     ONL2-B07(3:2)      TO      WK-TENCD
             MOVE     ONL2-B07           TO      WK-TENCD
*************#2017/04/17 NAV ED
     END-IF.
*    明細格納
     IF      ONL3-C02       =     "D1"
             MOVE     SPACE       TO     SGK-REC
             INITIALIZE                  SGK-REC
             MOVE     WK-SIMEBI   TO     SGK-F01
             MOVE     WK-TENCD    TO     SGK-F02
             MOVE     ONL3-C09    TO     SGK-F04
             MOVE     ONL3-C04    TO     SGK-F05
             MOVE     ONL3-C10    TO     SGK-F06
             MOVE     ONL3-C14    TO     SGK-F07
             MOVE     ONL3-C16    TO     SGK-F08
             MOVE     ONL3-C17    TO     SGK-F09
             MOVE     ONL3-C18    TO     SGK-F10
             WRITE    SGK-REC
     END-IF.
*    合計格納
     IF      ONL4-D02       =     "T1"
             MOVE     WK-SIMEBI   TO     SGG-F01
             MOVE     ONL4-D07    TO     SGG-F02
             MOVE     ONL4-D08    TO     SGG-F03
             MOVE     ONL4-D09    TO     SGG-F04
             MOVE     ONL4-D10    TO     SGG-F05
             MOVE     ONL4-D11    TO     SGG-F06
             MOVE     ONL4-D12    TO     SGG-F07
             WRITE    SGG-REC
     END-IF.
     ADD      1              TO   OUT-CNT.
     READ     JYOFUL
        AT    END
              MOVE      1    TO   END1-FLG
        NOT   AT END
              ADD       1    TO   IN-CNT
     END-READ.
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    JYOFUL     SITGK71     SITGG71.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI7103B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
