# SZI0090L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZI0090L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　在庫ＥＸＣＥＬ更新結果一覧表　    *
*    作成日／作成者　　　：　2016/07/05 INOUE                  *
*    処理概要　　　　　　：　在庫更新済みＥＸＣＥＬデータの　　*
*                            一覧表を出力する。　　　　　　　　*
*    変更日／変更者　　　：　2016/07/06 INOUE                  *
*    変更内容　　　　　　：　作業区分追加　G1 G5               *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SZI0090L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/07/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*在庫EXCEL累積ファイル
     SELECT   ZAIEXLR1  ASSIGN    TO        DA-01-VI-ZAIEXLR1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       EXL-F03
                                            EXL-F04
                                            EXL-F01
                                            EXL-F10
                                            EXL-F11
                        FILE  STATUS   IS   EXL-STATUS.
*担当者マスタ
     SELECT   TANMS1    ASSIGN     TO       DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS     MODE     RANDOM
                        RECORD     KEY      TAN-F01  TAN-F02
                        FILE  STATUS   IS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    在庫EXCEL累積ファイル
 FD  ZAIEXLR1           LABEL RECORD   IS   STANDARD.
     COPY     ZAIEXLR1  OF        XFDLIB
              JOINING   EXL       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  ZAIEXLR1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS          PIC  X(02).
     03  EXL-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
     03  SAK-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
     03  MEI-STATUS          PIC  X(02).
     03  PRT-STATUS          PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "SZI0090L".
         05  FILLER          PIC   X(11)  VALUE
                                          " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SZI0090L".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SZI0090L".
         05  FILLER          PIC   X(11)  VALUE
                                          " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SZI0090L".
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  FILLER          PIC  N(16)     VALUE
         NC"＜在庫ＥＸＣＥＬ更新結果一覧表＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(22)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-MS         PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"更新日付範囲".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD000-DATEF     PIC  X(10).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"～".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-DATET     PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"更新担当者".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD000-TANCD     PIC  X(07).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TANNM     PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)
                             VALUE  NC"条件→".
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04)
                             VALUE NC"棚移動：".
         05  HD000-JYOKEN1   PIC  N(01).
         05  FILLER          PIC  N(06)
                             VALUE NC"　ストック：".
         05  HD000-JYOKEN2   PIC  N(01).
         05  FILLER          PIC  N(06)
                             VALUE NC"　製品移動：".
         05  HD000-JYOKEN3   PIC  N(01).
         05  FILLER          PIC  N(04)
                             VALUE NC"　廃棄：".
         05  HD000-JYOKEN4   PIC  N(01).
         05  FILLER          PIC  N(05)
                             VALUE NC"　セット：".
         05  HD000-JYOKEN5   PIC  N(01).
         05  FILLER          PIC  N(05)
                             VALUE NC"　バラシ：".
         05  HD000-JYOKEN6   PIC  N(01).
*
 01  HD01.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"作業日".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"作業区分".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"部門".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"倉庫→倉庫".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(08)
                                  VALUE NC"サカタ商品コード".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04)   VALUE  NC"出庫棚番".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"入庫棚番".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"入出".
         05  FILLER          PIC  X(06)   VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(02)   VALUE  NC"数量".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"備考".
         05  FILLER          PIC  X(07)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"伝票_".
         05  FILLER          PIC  N(01)   VALUE  NC"－".
         05  FILLER          PIC  N(01)   VALUE  NC"行".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"未".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  X(06)   VALUE  "ｽﾄｯｸNO".
         05  FILLER          PIC  N(01)   VALUE  NC"→".
         05  FILLER          PIC  X(06)   VALUE  "ｽﾄｯｸNO".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"更新日".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SAGYOUBI   PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SKUBUN     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  MS01-SKUBUNNM   PIC  N(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BUMON      PIC  X(04).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-SOUKOF     PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SOUKOT     PIC  X(02).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-SYOCD      PIC  X(19).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANAS      PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANAN      PIC  X(06).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-IRIDE      PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SUURYOU    PIC  Z,ZZZ,ZZ9.
         05  MS01-SUURYOUX   REDEFINES  MS01-SUURYOU.
             07 MS01-SURYOUX PIC  X(09).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BIKOU      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DEN-GYO    PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS01-MISYUKKO   PIC  N(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-STOCKF     PIC  X(06).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-STOCKT     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KOUSINBI   PIC  X(10).
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　該当する累積データはありません　！！　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE                PIC  X(01)     VALUE  SPACE.
 01  P-LINE1                PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2                PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME               PIC  9(08).
 01  WK-TIME      REDEFINES SYS-TIME.
   03  WK-TIME-HM           PIC  9(06).
   03  WK-TIME-FIL          PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANCD          PIC   X(02).
 01  PARA-IN-DATEF          PIC   9(08).
 01  PARA-IN-DATET          PIC   9(08).
 01  PARA-IN-SKBN1          PIC   X(01).
 01  PARA-IN-SKBN2          PIC   X(01).
 01  PARA-IN-SKBN3          PIC   X(01).
 01  PARA-IN-SKBN4          PIC   X(01).
 01  PARA-IN-SKBN5          PIC   X(01).
 01  PARA-IN-SKBN6          PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-DATEF
                                       PARA-IN-DATET
                                       PARA-IN-SKBN1
                                       PARA-IN-SKBN2
                                       PARA-IN-SKBN3
                                       PARA-IN-SKBN4
                                       PARA-IN-SKBN5
                                       PARA-IN-SKBN6.
*---------------------------------------------------------------*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAIEXLR1.
     MOVE      "ZAIEXLR1"   TO   AB-FILE.
     MOVE      EXL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"    TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL   END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     ZAIEXLR1
                        TANMS1.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME          FROM   TIME.
*在庫EXCEL累積ファイルスタート
     PERFORM  ZAIEXLR1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　累積データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*在庫EXCEL累積ファイル読込
     PERFORM ZAIEXLR1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　累積データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    在庫EXCEL累積ファイルスタート
****************************************************************
 ZAIEXLR1-START-SEC          SECTION.
*
     MOVE    "ZAIEXLR1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   EXL-REC.
     INITIALIZE                        EXL-REC.
*
     MOVE     PARA-IN-BUMON       TO   EXL-F03.
     MOVE     PARA-IN-TANCD       TO   EXL-F04.
     MOVE     PARA-IN-DATEF       TO   EXL-F01.
     MOVE     ZERO                TO   EXL-F10.
     MOVE     ZERO                TO   EXL-F11.
*
     START  ZAIEXLR1  KEY  IS  >=      EXL-F03 EXL-F04
                                       EXL-F01 EXL-F10 EXL-F11
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 ZAIEXLR1-START-EXIT.
     EXIT.
*
****************************************************************
*    在庫EXCEL累積ファイル読込
****************************************************************
 ZAIEXLR1-READ-SEC           SECTION.
*
     MOVE    "ZAIEXLR1-READ-SEC"   TO   S-NAME.
*
     READ     ZAIEXLR1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAIEXLR1-READ-EXIT
     END-READ.
*更新部門・担当
     IF     ( EXL-F03   NOT =  PARA-IN-BUMON ) OR
            ( EXL-F04   NOT =  PARA-IN-TANCD )
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAIEXLR1-READ-EXIT
     END-IF.
*更新日範囲
     IF     ( EXL-F01       <  PARA-IN-DATEF ) OR
            ( EXL-F01       >  PARA-IN-DATET )
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAIEXLR1-READ-EXIT
     END-IF.
*作業区分
*  55:棚移動
     IF       PARA-IN-SKBN1 = " "
        IF    EXL-F05       = "55"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*  58:ストック_ＣＨＧ
     IF       PARA-IN-SKBN2 = " "
        IF    EXL-F05       = "58"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*  I3:製品在庫移動
     IF       PARA-IN-SKBN3 = " "
        IF    EXL-F05       = "I3"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*  46:廃棄
     IF       PARA-IN-SKBN4 = " "
        IF    EXL-F05       = "46"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*  G1:セット組
     IF       PARA-IN-SKBN5 = " "
        IF    EXL-F05       = "G1"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*  G5:バラシ
     IF       PARA-IN-SKBN6 = " "
        IF    EXL-F05       = "G5"
              GO   TO   ZAIEXLR1-READ-SEC
        END-IF
     END-IF.
*件数カウント
     ADD      1                   TO   ZAIEXLR1-READ-CNT.
*
 ZAIEXLR1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     PERFORM  ZAIEXLR1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    更新日範囲
     MOVE     PARA-IN-DATEF(1:4)  TO   HD000-DATEF(1:4).
     MOVE     "/"                 TO   HD000-DATEF(5:1).
     MOVE     PARA-IN-DATEF(5:2)  TO   HD000-DATEF(6:2).
     MOVE     "/"                 TO   HD000-DATEF(8:1).
     MOVE     PARA-IN-DATEF(7:2)  TO   HD000-DATEF(9:2).
*
     MOVE     PARA-IN-DATET(1:4)  TO   HD000-DATET(1:4).
     MOVE     "/"                 TO   HD000-DATET(5:1).
     MOVE     PARA-IN-DATET(5:2)  TO   HD000-DATET(6:2).
     MOVE     "/"                 TO   HD000-DATET(8:1).
     MOVE     PARA-IN-DATET(7:2)  TO   HD000-DATET(9:2).
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TANCD(1:4).
     MOVE     "-"                 TO           HD000-TANCD(5:1).
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TANCD(6:2).
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG  =   SPACE
              MOVE  TAN-F03       TO   HD000-TANNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-TANNM
     END-IF.
*    条件(出力選択）
     IF       PARA-IN-SKBN1       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN1
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN1
     END-IF.
     IF       PARA-IN-SKBN2       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN2
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN2
     END-IF.
     IF       PARA-IN-SKBN3       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN3
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN3
     END-IF.
     IF       PARA-IN-SKBN4       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN4
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN4
     END-IF.
     IF       PARA-IN-SKBN5       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN5
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN5
     END-IF.
     IF       PARA-IN-SKBN6       =    "Y"
              MOVE  NC"Ｙ"        TO   HD000-JYOKEN6
     ELSE
              MOVE  NC"　"        TO   HD000-JYOKEN6
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     8                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*   作業日
     MOVE     EXL-F09(1:4)        TO   MS01-SAGYOUBI(1:4).
     MOVE     "/"                 TO   MS01-SAGYOUBI(5:1).
     MOVE     EXL-F09(5:2)        TO   MS01-SAGYOUBI(6:2).
     MOVE     "/"                 TO   MS01-SAGYOUBI(8:1).
     MOVE     EXL-F09(7:2)        TO   MS01-SAGYOUBI(9:2).
*   作業区分
     MOVE     EXL-F05             TO   MS01-SKUBUN.
     EVALUATE EXL-F05
        WHEN  "I3"
              MOVE  NC"製品移動"  TO   MS01-SKUBUNNM
        WHEN  "46"
              MOVE  NC"廃棄　　"  TO   MS01-SKUBUNNM
        WHEN  "55"
              MOVE  NC"棚移動　"  TO   MS01-SKUBUNNM
        WHEN  "58"
              MOVE  NC"ストック"  TO   MS01-SKUBUNNM
        WHEN  "G1"
              MOVE  NC"セット組"  TO   MS01-SKUBUNNM
        WHEN  "G5"
              MOVE  NC"バラシ　"  TO   MS01-SKUBUNNM
        WHEN  OTHER
              MOVE  NC"？？？？"  TO   MS01-SKUBUNNM
     END-EVALUATE.
*   部門
     MOVE     EXL-F06             TO   MS01-BUMON.
*   倉庫（元）
     MOVE     EXL-F07             TO   MS01-SOUKOF.
*   倉庫（先）
     IF       EXL-F05    = "I3"
              MOVE    EXL-F08     TO   MS01-SOUKOT
     ELSE
              MOVE    SPACE       TO   MS01-SOUKOT
     END-IF.
*   サカタ商品コード
     MOVE     EXL-F12             TO   MS01-SYOCD(1:8).
     MOVE     "-"                 TO   MS01-SYOCD(9:1).
     MOVE     EXL-F13             TO   MS01-SYOCD(10:5).
     MOVE     "-"                 TO   MS01-SYOCD(15:1).
     MOVE     EXL-F14             TO   MS01-SYOCD(16:2).
     MOVE     "-"                 TO   MS01-SYOCD(18:1).
     MOVE     EXL-F15             TO   MS01-SYOCD(19:1).
*   出庫棚番
     MOVE     EXL-F18             TO   MS01-TANAS.
*   入庫棚番
     MOVE     EXL-F19             TO   MS01-TANAN.
*   入出庫区分
     IF       EXL-F05    =  "G1"  OR   "G5"
        IF    EXL-F23    =  "1"
              MOVE  NC"入"        TO   MS01-IRIDE
        ELSE
              MOVE  NC"出"        TO   MS01-IRIDE
        END-IF
     ELSE
              MOVE  NC"　"        TO   MS01-IRIDE
     END-IF.
*   数量
     MOVE     EXL-F20             TO   MS01-SUURYOU.
*   備考
     MOVE     EXL-F22             TO   MS01-BIKOU.
*   伝票_-行
     MOVE     SPACE               TO   MS01-DEN-GYO.
     MOVE     EXL-F10             TO   MS01-DEN-GYO(1:7).
     IF       EXL-F05    =  "46"  OR   "I3"  OR  "G1"  OR  "G5"
              MOVE  "-"           TO   MS01-DEN-GYO(8:1)
              MOVE  EXL-F11       TO   MS01-DEN-GYO(9:2)
     END-IF.
*   未出庫ＦＬＧ
     MOVE     NC"　"              TO   MS01-MISYUKKO.
     IF       EXL-F05    =  "55"
        IF    EXL-F21    =  "1"
              MOVE  NC"未"        TO   MS01-MISYUKKO
        END-IF
     END-IF.
*   ストック（元）
     MOVE     EXL-F16             TO   MS01-STOCKF.
*   ストック（先）
     MOVE     EXL-F17             TO   MS01-STOCKT.
*   更新日
     MOVE     EXL-F01(1:4)        TO   MS01-KOUSINBI(1:4).
     MOVE     "/"                 TO   MS01-KOUSINBI(5:1).
     MOVE     EXL-F01(5:2)        TO   MS01-KOUSINBI(6:2).
     MOVE     "/"                 TO   MS01-KOUSINBI(8:1).
     MOVE     EXL-F01(7:2)        TO   MS01-KOUSINBI(9:2).
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*在庫EXCEL累積ファイル読込
     DISPLAY "ZAIEXLR1 PRINT-CNT = " ZAIEXLR1-READ-CNT UPON CONS.
*
     CLOSE     ZAIEXLR1  TANMS1  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
