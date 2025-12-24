****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコト新ＥＤＩシステム　　　　　*
*    業務名　　　　　　　：　受領処理　　　　　　              *
*    モジュール名　　　　：　受領書ＣＳＶデータ出力　          *
*    作成日／更新日　　　：　10/10/07                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ナフコ受領書データをＣＳＶに　　　*
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3769B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/07.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<ナフコ出荷指示Ｆ >>********************************
     SELECT   NFJYURF           ASSIGN    TO   DA-01-VI-NFJYURL4
                                ORGANIZATION   INDEXED
                                ACCESS  MODE   SEQUENTIAL
                                RECORD  KEY    NFJ-F01  NFJ-F05
                                               NFJ-F03  NFJ-F04
                                               NFJ-F07  NFJ-F08
                                               NFJ-F02
                                STATUS         NFJ-STATUS.
*
****<<受領アンマッチデータ　 >>*********************************
     SELECT   NFUNMAF            ASSIGN    TO   DA-01-VI-NFUNMAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    UNM-F01  UNM-F02
                                                UNM-F03
                                 STATUS         UNM-STATUS.
*
*商品名称マスタ
     SELECT   NFSHOMS    ASSIGN    TO       DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE STATUS    IS   MEI-STATUS.
*
*****<<出荷指示ﾃﾞｰﾀCSV >>*************************************
     SELECT   NFJYRDT           ASSIGN    TO   NFJYRDT
                                STATUS         NFD-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ出荷指示データ                               *
*--------------------------------------------------------------*
 FD  NFJYURF            LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   NFJ       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　受領アンマッチデータ                     *
*--------------------------------------------------------------*
 FD  NFUNMAF            LABEL RECORD   IS   STANDARD.
     COPY     NFUNMAF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*
*--------------------------------------------------------------*
*    商品名称マスタ
*--------------------------------------------------------------*
 FD  NFSHOMS             LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ受領データＣＳＶ　　　　　                   *
*--------------------------------------------------------------*
 FD  NFJYRDT           BLOCK CONTAINS 1    RECORDS.
 01  NFD-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  NFJ-STATUS                   PIC       X(02).
 01  NFD-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
 01  UNM-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
***** 店舗テーブル
 01  TBL-TENPO.
     03  TBL-TENPOR               OCCURS    2000.
         05   TBL-F01             PIC       9(05)  VALUE  ZERO.
         05   TBL-F02             PIC       9(03)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"データ受信日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"仕入計上日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"赤黒区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗名称".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"元伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"商品名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(02)  VALUE  NC"数量".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"原単価（税抜）".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"売単価（税抜）".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"受領区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"受領区分名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"理由区分".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"理由ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"理由名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"連絡事項".
     03  FILLER        PIC X(01)  VALUE  X"29".

*明細エリア
 01  WK-MEISAI.
     03  NFD-F01       PIC 9(08).
     03  NFD-A01       PIC X(01).
     03  NFD-F02       PIC 9(08).
     03  NFD-A02       PIC X(01).
     03  NFD-F03       PIC 9(02).
     03  NFD-A03       PIC X(01).
     03  NFD-F04       PIC 9(02).
     03  NFD-A04       PIC X(01).
     03  NFD-F05       PIC X(03).
     03  NFD-A05       PIC X(01).
     03  NFD-F06       PIC X(15).
     03  NFD-A06       PIC X(01).
     03  NFD-F07       PIC 9(08).
     03  NFD-A07       PIC X(01).
     03  NFD-F08       PIC 9(02).
     03  NFD-A08       PIC X(01).
     03  NFD-F09       PIC 9(08).
     03  NFD-A09       PIC X(01).
     03  NFD-F10       PIC 9(08).
     03  NFD-A10       PIC X(01).
     03  NFD-F11       PIC X(13).
     03  NFD-A11       PIC X(01).
     03  NFD-AN01      PIC X(01).
     03  NFD-F12S      PIC X(01).
     03  NFD-F12       PIC N(40).
     03  NFD-F12E      PIC X(01).
     03  NFD-AN02      PIC X(01).
     03  NFD-A12       PIC X(01).
     03  NFD-F13       PIC 9(06).
     03  NFD-A13       PIC X(01).
     03  NFD-F14       PIC 9(09).
     03  NFD-A14       PIC X(01).
     03  NFD-F15       PIC 9(09).
     03  NFD-A15       PIC X(01).
     03  NFD-F16       PIC 9(02).
     03  NFD-A16       PIC X(01).
     03  NFD-F161A     PIC X(01).
     03  NFD-F161      PIC N(10).
     03  NFD-F161B     PIC X(01).
     03  NFD-A161      PIC X(01).
     03  NFD-F17       PIC 9(02).
     03  NFD-A17       PIC X(01).
     03  NFD-F18       PIC 9(02).
     03  NFD-A18       PIC X(01).
     03  NFD-F191      PIC X(01).
     03  NFD-F19       PIC N(20).
     03  NFD-F192      PIC X(01).
     03  NFD-A19       PIC X(01).
     03  NFD-F201      PIC X(01).
     03  NFD-F20       PIC N(30).
     03  NFD-F202      PIC X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3769B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3769B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3769B".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  LINK-IN-STYMD      PIC  9(08).
 01  LINK-IN-EDYMD      PIC  9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-IN-STYMD
                                         LINK-IN-EDYMD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJYURF.
     MOVE     "NFJYURF "          TO        ERR-FL-ID.
     MOVE     NFJ-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSHOMS.
     MOVE     "NFSHOMS"            TO        ERR-FL-ID.
     MOVE     MEI-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFUNMAF.
     MOVE     "NFUNMAF"           TO        ERR-FL-ID.
     MOVE     UNM-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJYRDT.
     MOVE     "NFJYRDT"          TO        ERR-FL-ID.
     MOVE     NFD-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3769B-START         SECTION.
*
     MOVE   "SSY3769B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     IF   END-FLG  NOT =  "END"
          PERFORM    MAIN-SEC  UNTIL  END-FLG   =  "END"
     END-IF.
*****PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY3769B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFJYURF.
     OPEN     INPUT     NFSHOMS.
     OPEN     INPUT     NFUNMAF.
     OPEN     OUTPUT    NFJYRDT.
     DISPLAY  MSG-START UPON CONS.
     DISPLAY "LINK-IN-STYMD = "  LINK-IN-STYMD  UPON  CONS.
     DISPLAY "LINK-IN-EDYMD = "  LINK-IN-EDYMD  UPON  CONS.
*
     MOVE     SPACE               TO   NFJ-REC.
     INITIALIZE                        NFJ-REC.
     MOVE     LINK-IN-STYMD       TO   NFJ-F01.
*
     START    NFJYURF   KEY  >=   NFJ-F01  NFJ-F05
                                  NFJ-F03  NFJ-F04
                                  NFJ-F07  NFJ-F08
                                  NFJ-F02
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   INIT-EXIT
     END-START.
*
     PERFORM  NFJYURF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ受領データ読込　　　　　　
****************************************************************
 NFJYURF-RD-SEC             SECTION.
*
     MOVE    "NFJYURF-RD-SEC"    TO   S-NAME.
*
     READ     NFJYURF
          AT END
              MOVE     "END"      TO        END-FLG
              GO    TO      NFJYURF-RD-EXIT
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
     IF     NFJ-F01   >    LINK-IN-EDYMD
            MOVE     "END"      TO        END-FLG
            GO                  TO        NFJYURF-RD-EXIT
     END-IF.
*
 NFJYURF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   WK-MEISAI.
     INITIALIZE                        WK-MEISAI.
*項目転送
     MOVE   X"28"                 TO   NFD-AN01.
     MOVE   X"29"                 TO   NFD-AN02.
*
     MOVE     ","                 TO   NFD-A01 NFD-A02
                                       NFD-A03 NFD-A04
                                       NFD-A05 NFD-A06
                                       NFD-A07 NFD-A08
                                       NFD-A09 NFD-A10
                                       NFD-A11 NFD-A12
                                       NFD-A13 NFD-A14
                                       NFD-A15 NFD-A16 NFD-A161
                                       NFD-A17 NFD-A18.
*
     MOVE     NFJ-F01      TO   NFD-F01.
     MOVE     NFJ-F02      TO   NFD-F02.
     MOVE     NFJ-F03      TO   NFD-F03.
     MOVE     NFJ-F04      TO   NFD-F04.
     MOVE     NFJ-F05      TO   NFD-F05.
     MOVE     NFJ-F06      TO   NFD-F06.
     MOVE     NFJ-F07      TO   NFD-F07.
     MOVE     NFJ-F08      TO   NFD-F08.
     MOVE     NFJ-F09      TO   NFD-F09.
     MOVE     NFJ-F10      TO   NFD-F10.
     MOVE     NFJ-F11      TO   NFD-F11.
*
     MOVE     NFJ-F10      TO   MEI-F01.
*
     MOVE     X"28"        TO   NFD-F12S.
     MOVE     X"29"        TO   NFD-F12E.
*
     READ     NFSHOMS
          INVALID   KEY
              MOVE  NC"商品マスタ未登録です！！"
                                  TO   NFD-F12
          NOT INVALID   KEY
              MOVE  MEI-F05       TO   NFD-F12(1:10)
              MOVE  MEI-F06       TO   NFD-F12(11:10)
     END-READ.
*
     IF       OUTPUT-CNT    =   ZERO
              MOVE     WK-HEAD      TO   NFD-REC
              WRITE    NFD-REC
              ADD      1            TO   OUTPUT-CNT
     END-IF.
*
     MOVE     NFJ-F12      TO   NFD-F13.
*****COMPUTE  NFD-F13     =   NFJ-F12  /  10.
     EVALUATE NFJ-F03  ALSO  NFJ-F04
         WHEN 1  ALSO  1  MOVE "-"        TO  NFD-F13(1:1)
         WHEN 2  ALSO  1  MOVE "-"        TO  NFD-F13(1:1)
         WHEN 11 ALSO  0  MOVE "-"        TO  NFD-F13(1:1)
         WHEN 12 ALSO  0  MOVE "-"        TO  NFD-F13(1:1)
     END-EVALUATE.
     MOVE     NFJ-F13      TO   NFD-F14.
     MOVE     NFJ-F14      TO   NFD-F15.
     MOVE     NFJ-F15      TO   NFD-F16.
     MOVE     SPACE                TO        NFD-F161.
     MOVE     X"28"                TO        NFD-F161A.
     MOVE     X"29"                TO        NFD-F161B.
     EVALUATE NFJ-F15
         WHEN 0
         MOVE NC"返品／値引伝票　　　"  TO   NFD-F161
         WHEN 1
         MOVE NC"予定通り受領しました"  TO   NFD-F161
         WHEN 2
         MOVE NC"入荷修正を実施　受領"  TO   NFD-F161
         WHEN 3
         MOVE NC"仕入訂正（赤黒）　　"  TO   NFD-F161
     END-EVALUATE.
     MOVE     NFJ-F16      TO   NFD-F17.
     MOVE     NFJ-F17      TO   NFD-F18.
     MOVE     X"28"        TO   NFD-F191.
     MOVE     NFJ-F18      TO   NFD-F19.
     EVALUATE NFJ-F16 ALSO NFJ-F17
        WHEN  1       ALSO 1
        MOVE NC"ＴＡ／訂正理由　　仕入先都合　　　　　　"
                                           TO  NFD-F19
        WHEN  1       ALSO 2
        MOVE NC"ＴＡ／訂正理由　　ナフコ都合　　　　　　"
                                           TO  NFD-F19
        WHEN  1       ALSO 3
        MOVE NC"ＴＡ／訂正理由　　その他　　　　　　　　"
                                           TO  NFD-F19
        WHEN  2       ALSO 1
        MOVE NC"修正（赤伝）理由　不良品　　　　　　　　"
                                           TO  NFD-F19
        WHEN  2       ALSO 99
        MOVE NC"修正（赤伝）理由　仕入先都合　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 1
        MOVE NC"修正（赤伝）理由　店舗ＣＤ違い　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 2
        MOVE NC"修正（赤伝）理由　仕入先違い　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 3
        MOVE NC"修正（赤伝）理由　日付違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 4
        MOVE NC"修正（赤伝）理由　原価違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 5
        MOVE NC"修正（赤伝）理由　売価違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 6
        MOVE NC"修正（赤伝）理由　数量違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 7
        MOVE NC"修正（赤伝）理由　金額違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 8
        MOVE NC"修正（赤伝）理由　部門違い　　　　　　　"
                                           TO  NFD-F19
        WHEN  3       ALSO 99
        MOVE NC"修正（赤伝）理由　商品ＣＤ違い　　　　　"
                                           TO  NFD-F19
        WHEN  7       ALSO 1
        MOVE NC"仕入値引理由（買掛）　ＶＤ仮引　　　　　"
                                           TO  NFD-F19
        WHEN  7       ALSO 2
        MOVE NC"仕入値引理由（買掛）　ＶＤ清算　　　　　"
                                           TO  NFD-F19
        WHEN  7       ALSO 3
        MOVE NC"仕入値引理由（買掛）　ＶＤ前期末清算　　"
                                           TO  NFD-F19
        WHEN  8       ALSO 4
        MOVE NC"仕入値引理由（本部発生）　ＶＤ清算　　　"
                                           TO  NFD-F19
        WHEN  8       ALSO 5
        MOVE NC"仕入値引理由（本部発生）　違算調整　　　"
                                           TO  NFD-F19
        WHEN  8       ALSO 99
        MOVE NC"仕入値引理由（本部発生）　本部指示　　　"
                                           TO  NFD-F19
        WHEN  9       ALSO 99
        MOVE NC"仕入値引理由（店舗発生）　本部指示　　　"
                                           TO  NFD-F19
        WHEN  14      ALSO 1
        MOVE NC"検品差異理由　数量不足　　　　　　　　　"
                                           TO  NFD-F19
        WHEN  14      ALSO 2
        MOVE NC"検品差異理由　不良品　　　　　　　　　　"
                                           TO  NFD-F19
     END-EVALUATE.
     MOVE     X"29"        TO   NFD-F192.
*連絡事項
     MOVE     NFJ-F19              TO        NFD-F20.
     MOVE     X"28"                TO        NFD-F201.
     MOVE     X"29"                TO        NFD-F202.
*レコード転送
     MOVE     WK-MEISAI    TO   NFD-REC.
*
     WRITE    NFD-REC
     ADD      1            TO   OUTPUT-CNT
*    次レコード読込み
     PERFORM  NFJYURF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    NFJYURF   NFSHOMS  NFJYRDT  NFUNMAF.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
