****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿                  *
*    サブシステム　　　　：　富岡　_番表                      *
*    業務名　　　　　　　：　富岡　_番表                      *
*    モジュール名　　　　：　_番表出力                        *
*    作成日／更新日　　　：　06/05/24                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　_番データを読み、_番表を出力する*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             STN0030L.
 AUTHOR.                 NAV.
 DATE-WRITTEN.           06.05.24.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
 INPUT-OUTPUT            SECTION.
 FILE-CONTROL.
*    _番データ
     SELECT   ZTANAF       ASSIGN     TO      DA-01-VI-ZTANAL1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    SEQUENTIAL
                           RECORD     KEY     TAN-F01
                                              TAN-F02
                                              TAN-F03
                                              TAN-F04
                           FILE    STATUS     TAN-STATUS.
*    プリントファイル
     SELECT  PRTF      ASSIGN    TO             GS-PRTF
                       DESTINATION              "PRT"
                       FORMAT                   PRT-FORM
                       GROUP                    PRT-GRP
                       PROCESSING               PRT-PROC
                       UNIT CONTROL             PRT-CTL
                       FILE STATUS              PRT-STATUS.
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*    _番データ
 FD  ZTANAF
     BLOCK       CONTAINS   40       RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZTANAF    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTN00301  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  TAN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
*    ＦＯＲＭ制御領域
 01  PRT-FORM                    PIC  X(08).
 01  PRT-PROC                    PIC  X(02).
 01  PRT-GRP                     PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL               PIC  X(04).
     03  PRT-STR-PG              PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
     03  FLG-TK                   PIC  X(02)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-NOUDATE               PIC  9(08)  VALUE  ZERO.
     03  WK-SYOCD                 PIC  X(13)  VALUE  ZERO.
     03  WK-SYUKA                 PIC  X(02)  VALUE  SPACE.
     03  WK-RUTO                  PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  YOKO                     PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  IZ                       PIC  9(01)  VALUE  ZERO.
     03  WK-HENKAN                PIC  N(01)  VALUE  SPACE.
     03  CHK-FLG                  PIC  9(01)  VALUE  ZERO.
     03  WK-GK-SURYO              PIC  9(06)  VALUE  ZERO.
     03  WK-DATE                  PIC  9(01)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)  VALUE  ZERO.
     03  INJI-FLG                 PIC  9(01)  VALUE  ZERO.
     03  FLG-OK                   PIC  X(02)  VALUE  SPACE.
*    ルート毎店舗情報ワークエリア
 01  WK-TENPO.
     03  TENPO                    OCCURS      13.
         05  TENCD                PIC  9(03)  VALUE  ZERO.
         05  TENMEI               PIC  N(03)  VALUE  SPACE.
*    ルート番号退避
 01  WK-TAN-REC.
     03  WK-TAN-F02               PIC  N(06)  VALUE  SPACE.
     03  WK-TAN-F03               PIC  N(08)  VALUE  SPACE.
     03  WK-TAN-F02-A             PIC  N(07)  VALUE  SPACE.
*----<< EBCDIC -> JEF CONV >>-*
 01  EBCDIC-TBL.
     03  FILLER              PIC  X(25)     VALUE
         "1234567890 ABCDEFGHIJKLMN".
     03  FILLER              PIC  X(12)     VALUE
         "OPQRSTUVWXYZ".
 01  EBCDIC-TABLE-X          REDEFINES      EBCDIC-TBL.
     03  EBCDIC-TABLE-XX     OCCURS    37
                             INDEXED   BY   IXE.
         05  EBCDIC-TABLE    PIC  X(01).
 01  JEF-TBL.
     03  FILLER         PIC  N(25)     VALUE
         NC"１２３４５６７８９０　ＡＢＣＤＥＦＧＨＩＪＫＬＭＮ".
     03  FILLER              PIC  N(12)     VALUE
         NC"ＯＰＱＲＳＴＵＶＷＸＹＺ".
 01  JEF-TABLE-X        REDEFINES      JEF-TBL.
     03  JEF-TABLE-XX        OCCURS    37
                             INDEXED   BY   IXJ.
         05  JEF-TABLE  PIC  N(01).
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
*    漢字ワークエリア
 01  WK-HENKAN-KANJI.
     03  WK-HENK1.
         05  WK-HENK11            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK12            PIC  N(01)  VALUE  SPACE.
     03  WK-HENK2.
         05  WK-HENK21            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK22            PIC  N(01)  VALUE  SPACE.
     03  WK-HENK3.
         05  WK-HENK31            PIC  N(01)  VALUE  SPACE.
         05  WK-HENK32            PIC  N(01)  VALUE  SPACE.
*    合計金額格納エリア
 01  GOKEI-AREA.
     03  GOKEI-GENKA              OCCURS      13     TIMES.
         05  GENKEI               PIC S9(09).
     03  GOKEI-BAIKA              OCCURS      13     TIMES.
         05  BAIKEI               PIC S9(09).
*    商品名編集エリア
 01  SYOHIN-AREA.
     03  SYOHIN-1.
         05  WK-SYOHIN-11         PIC  X(14).
         05  WK-SYOHIN-12         PIC  X(01).
     03  SYOHIN-2.
         05  WK-SYOHIN-21         PIC  X(01).
         05  WK-SYOHIN-22         PIC  X(14).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "STN0030L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                 SECTION.
 01  LINK-SOKCD            PIC X(02).
 01  LINK-TANAST           PIC X(06).
 01  LINK-TANAED           PIC X(06).
*============================================================*
 PROCEDURE               DIVISION  USING  LINK-SOKCD
                                          LINK-TANAST
                                          LINK-TANAED.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 FILEERR-SEC1           SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTF.
     MOVE      "PRTF"           TO   ERR-FL-ID.
     MOVE      PRT-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　_番データ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   ZTANAF.
     MOVE      "ZTANAF"         TO   ERR-FL-ID.
     MOVE      TAN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造_0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
     MOVE     "COTROL-SEC"        TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    FLG-END  =  "END".
     PERFORM  END-SEC.
     STOP     RUN.
*
 CONTROL-EXIT.
     EXIT.
*============================================================*
*　　初期処理　　　　　　　　　　　　　　  構造_1.0         *
*============================================================*
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT      ZTANAF.
     OPEN     OUTPUT     PRTF.
*    システム日付の取得
*システム日付・時刻の取得
     ACCEPT   WRK-DATE2         FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WRK-DATE2           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE.
*    プリントエリア初期化
     MOVE     SPACE      TO       PRT-FTN00301.
     MOVE     ZERO       TO       READ-CNT.
     MOVE     ZERO       TO       GOKEI-AREA.
     MOVE     SPACE      TO       FLG-TK.
*    _番データスタート
     MOVE     SPACE      TO       TAN-REC.
     INITIALIZE                   TAN-REC.
     MOVE    LINK-SOKCD  TO       TAN-F01.
     MOVE    LINK-TANAST TO       TAN-F02.
     START  ZTANAF  KEY  IS  >=  TAN-F01 TAN-F02 TAN-F03 TAN-F04
            INVALID
            MOVE   "END" TO       FLG-END
            DISPLAY NC"＃対象データ無し＃" UPON CONS
            GO           TO       INIT-EXT
     END-START.
*
     PERFORM  ZTANAF-READ-SEC.
*
     MOVE     1          TO       TATE  YOKO.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　_番データ読込み
*============================================================*
 ZTANAF-READ-SEC       SECTION.
     MOVE     "ZTANAF-READ-SEC"   TO   S-NAME.
*
     READ     ZTANAF   AT  END
              MOVE     "END"      TO       FLG-END
              GO                  TO       ZTANAF-READ-EXIT
              NOT  AT  END
              ADD       1         TO       READ-CNT
     END-READ.
*件数表示
     IF       READ-CNT(4:3) = "000" OR "500"
              DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*_番範囲チェック
     IF       TAN-F02  >  LINK-TANAED
              MOVE     "END"      TO       FLG-END
              GO                  TO       ZTANAF-READ-EXIT
     END-IF.
*
 ZTANAF-READ-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*縦横添字チェック
     IF     TATE  =  3
*           _番表を出力する。
*           印字制御項目セット
            MOVE     SPACE               TO   PRT-PROC
            MOVE     SPACE               TO   PRT-CTL
            MOVE     SPACE               TO   PRT-FORM
            MOVE    "FTN00301"           TO   PRT-FORM
            MOVE    "SCREEN"             TO   PRT-GRP
            WRITE   PRT-FTN00301
            MOVE     SPACE               TO   PRT-FTN00301
            MOVE     1                   TO   TATE  YOKO
            MOVE     ZERO                TO   INJI-FLG
     END-IF.
*項目転送
     MOVE   NC"_　番　票"     TO   PRT-TAITL(TATE YOKO).
*_番日本語変換
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
        MOVE    SPACE      TO     FLG-OK
        PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 37
                                          OR FLG-OK = "OK"
             IF   TAN-F02(IX:1) = EBCDIC-TABLE(IY)
                  MOVE JEF-TABLE(IY)    TO WK-TAN-F02(IX:1)
                  MOVE "OK"             TO FLG-OK
             END-IF
        END-PERFORM
     END-PERFORM.
     MOVE   SPACE              TO   WK-TAN-F02-A.
     MOVE   WK-TAN-F02(1:4)    TO   WK-TAN-F02-A(1:4).
     MOVE   NC"－"             TO   WK-TAN-F02-A(5:1).
     MOVE   WK-TAN-F02(5:2)    TO   WK-TAN-F02-A(6:2).
     MOVE   WK-TAN-F02-A       TO   PRT-TANA(TATE YOKO).
*商品日本語変換
     MOVE   TAN-F05            TO   PRT-SYONM1(TATE YOKO).
     MOVE   TAN-F06            TO   PRT-SYONM2(TATE YOKO).
*商品日本語変換
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 8
        MOVE    SPACE      TO     FLG-OK
        PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 37
                                          OR FLG-OK = "OK"
             IF   TAN-F03(IX:1) = EBCDIC-TABLE(IY)
                  MOVE JEF-TABLE(IY)    TO WK-TAN-F03(IX:1)
                  MOVE "OK"             TO FLG-OK
             END-IF
        END-PERFORM
     END-PERFORM.
     MOVE   WK-TAN-F03         TO   PRT-SYOCD(TATE YOKO).
     MOVE   1                  TO   INJI-FLG.
     ADD    1                  TO   PAGE-CNT.
*
     ADD    1        TO       YOKO.
     IF     YOKO = 3
            ADD    1 TO       TATE
            MOVE   1 TO       YOKO
     END-IF.
*
     PERFORM ZTANAF-READ-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*
     IF     INJI-FLG = 1
*           _番表を出力する。
*           印字制御項目セット
            MOVE     SPACE               TO   PRT-PROC
            MOVE     SPACE               TO   PRT-CTL
            MOVE     SPACE               TO   PRT-FORM
            MOVE    "FTN00301"           TO   PRT-FORM
            MOVE    "SCREEN"             TO   PRT-GRP
            WRITE    PRT-FTN00301
     END-IF.
*    使用ファイルＣＬＯＳＥ
     CLOSE                        ZTANAF
                                  PRTF.
*    終了メッセージ
     DISPLAY "************************" UPON  CONS.
     DISPLAY "*    _番票出力枚数    *" UPON  CONS.
     DISPLAY "*  ｼｭﾂﾘｮｸ ﾏｲｽｳ = " PAGE-CNT "  *"  UPON  CONS.
     DISPLAY "************************" UPON  CONS.
*
 END-EXT.
     EXIT.
