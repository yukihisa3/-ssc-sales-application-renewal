****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　山新オンライン　　　　　　　　　　*
*　　モジュール名　　　　：　支払合計ファイル作成　　　　　　　*
*　　作成日／更新日　　　：　00/08/02                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　支払データより支払合計ファイルを　*
*　　　　　　　　　　　　：　作成する（ＰＣ用）                *
*　　更新日　　　　　　　：　2012/09/04                        *
*　　更新者　　　　　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　修正概要　　　　　　：　新レイアウト対応　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI3301B.
 AUTHOR.                Y.YOSHIDA.
 DATE-WRITTEN.          00/08/02.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払明細ファイル >>--*
**** SELECT   YAMAF     ASSIGN         DA-01-S-YAMAF
     SELECT   YAMAF     ASSIGN         DA-01-S-MEIDYAMA
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< 支払合計ファイル >>--*
**** SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
     SELECT   HSHIGKF   ASSIGN         DA-01-S-SITGKFE
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  YAMAF              LABEL     RECORD    IS   STANDARD
     BLOCK              CONTAINS      63    RECORDS.
 01  ONL1-REC.
         05  ONL1-A01             PIC  9(07).       *>仕入先
         05  ONL1-A02             PIC  9(06).       *>店舗
         05  ONL1-A03             PIC  9(08).       *>伝票番号
         05  ONL1-A04             PIC  9(02).       *>伝票区分
         05  ONL1-A05             PIC  9(11).       *>金額
         05  ONL1-A06             PIC  9(06).       *>年月度
         05  ONL1-FIL             PIC  X(24).       *>空白
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
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
     03  FH-CNT         PIC  9(06).
     03  MH-CNT         PIC  9(06).
     03  MS-CNT         PIC  9(06).
     03  GK-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
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
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC 9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 集信データ >>--*
 CVCSK-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YAMAF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI3301B YAMAF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI3301B HSHIGKF ERROR " SHI-ST " "
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
     DISPLAY  "*** SSI3301B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     YAMAF.
     OPEN     OUTPUT    HSHIGKF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     READ     YAMAF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
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
     MOVE     SPACE          TO   SHI-REC.
     INITIALIZE                   SHI-REC.
     MOVE     ONL1-A01       TO   SHI-F01.
     MOVE     ONL1-A02       TO   SHI-F02.
     MOVE     ONL1-A03       TO   SHI-F03.
     MOVE     ONL1-A04       TO   SHI-F04.
     MOVE     ONL1-A05       TO   SHI-F05.
     MOVE     ONL1-A06       TO   SHI-F06.
     MOVE     ONL1-FIL       TO   SHI-FIL.
*****MOVE     ALL "0"        TO   SHI-FIL.
*****DISPLAY "ONL1-A01 = " ONL1-A01 " - " SHI-F01 UPON CONS.
     WRITE    SHI-REC.
     ADD      1              TO   OUT-CNT.
*
 MAIN-01.
*
     READ     YAMAF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    YAMAF.
     CLOSE    HSHIGKF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
*## 2008/08/28 内部統制対応
     MOVE     OUT-CNT        TO   PARA-OUT-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI3301B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
