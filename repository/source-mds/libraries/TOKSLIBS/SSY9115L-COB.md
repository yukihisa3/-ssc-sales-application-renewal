# SSY9115L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY9115L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　カーマオンライン                  *
*    業務名　　　　　　　：　センター納品対応　　　　　　　　　*
*    モジュール名　　　　：　欠品情報報告書　　                *
*    作成日／更新日　　　：　17/03/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：                                    *
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SSY9115L.
 AUTHOR.                 NAV
 DATE-WRITTEN.           17.03.07.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE        IS             CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    欠品ファイル
     SELECT   KMKEPNF   ASSIGN        TO    DA-01-VI-KMKEPNL1
                        ORGANIZATION        INDEXED
                        ACCESS        MODE  SEQUENTIAL
                        RECORD        KEY   KPN-F46
                                            KPN-F47
                                            KPN-F01
                                            KPN-F48
                                            KPN-F07
                                            KPN-F12
                                            KPN-F25
                        FILE     STATUS     KPN-STATUS.
*    店舗マスタ
     SELECT   HTENMS    ASSIGN        TO    DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS        MODE  RANDOM
                        RECORD        KEY   TEN-F52  TEN-F011
                        FILE     STATUS     TEN-STATUS.
*    プリントファイル
     SELECT   PRTF      ASSIGN        TO    GS-PRTF
                        DESTINATION        "PRT"
                        FORMAT              PRT-FORM
                        GROUP               PRT-GRP
                        PROCESSING          PRT-PROC
                        UNIT CONTROL        PRT-CTL
                        FILE STATUS         PRT-STATUS.
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                   SECTION.
*    欠品ファイル
 FD  KMKEPNF
     LABEL       RECORD    IS        STANDARD.
     COPY        KMSHIRED  OF        XFDLIB
     JOINING     KPN       AS        PREFIX.
*    店舗マスタ
 FD  HTENMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY91151  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  KPN-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
*    ＦＯＲＭ制御領域
 01  PRT-FORM                     PIC  X(08).
 01  PRT-PROC                     PIC  X(02).
 01  PRT-GRP                      PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL                PIC  X(04).
     03  PRT-STR-PG               PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-SOKO                  PIC  X(02)  VALUE  SPACE.
     03  WK-CENTCD                PIC  9(05)  VALUE  ZERO.
     03  WK-BUMON                 PIC  9(03)  VALUE  ZERO.
     03  WK-HINM1                 PIC  X(20)  VALUE  SPACE.
     03  WK-HINM2                 PIC  X(20)  VALUE  SPACE.
     03  WK-SYOHIN                PIC  X(13)  VALUE  SPACE.
     03  WK-JANCD                 PIC  X(13)  VALUE  SPACE.
     03  WK-JYUTYU                PIC  9(09)  VALUE  ZERO.
     03  WK-SYUKA                 PIC  9(09)  VALUE  ZERO.
     03  WK-TORIHIKI              PIC  9(08)  VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)  VALUE  ZERO.
     03  SYUKEI-CNT               PIC  9(06)  VALUE  ZERO.
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
 01  WRK-DATE-YMD.
     03  WRK-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  WRK-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE-DD              PIC  9(02)  VALUE  ZERO.
*
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SSY9115L".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-CNTCD1            PIC   9(05).
 01  PARA-CNTCD2            PIC   9(05).
*============================================================*
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-CNTCD1
                                       PARA-CNTCD2.
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
*　　欠品データ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   KMKEPNF.
     MOVE      "KMKEPNF"        TO   ERR-FL-ID.
     MOVE      KPN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　店舗マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTENMS.
     MOVE      "HTENMS"         TO   ERR-FL-ID.
     MOVE      TEN-STATUS       TO   ERR-STCD.
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
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT     KMKEPNF  HTENMS.
     OPEN     OUTPUT    PRTF.
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
     MOVE     LINK-OUT-YMD        TO   WRK-DATE.
     MOVE     WRK-DATE            TO   WRK-DATE-YMD.
*    プリントエリア初期化
     MOVE     SPACE          TO   PRT-FSY91151.
     MOVE     ZERO           TO   READ-CNT.
*
*    欠品データスタート
     MOVE     SPACE          TO   KPN-REC.
     INITIALIZE                   KPN-REC.
     MOVE     PARA-JDATE     TO   KPN-F46.
     MOVE     PARA-JTIME     TO   KPN-F47.
     MOVE     PARA-TORICD    TO   KPN-F01.
     MOVE     PARA-SOKO      TO   KPN-F48.
     MOVE     PARA-CNTCD1    TO   KPN-F07.
     START    KMKEPNF   KEY  >=   KPN-F46   KPN-F47
                                  KPN-F01   KPN-F48
                                  KPN-F07   KPN-F12
                                  KPN-F25
         INVALID   KEY
              DISPLAY "START INV" UPON CONS
              DISPLAY "PARA JDATE=" PARA-JDATE  UPON CONS
              DISPLAY "PARA JTIME=" PARA-JTIME  UPON CONS
              DISPLAY "PARA TOKCD=" PARA-TORICD UPON CONS
              DISPLAY "PARA SOKO =" PARA-SOKO   UPON CONS
              MOVE    "END"  TO   FLG-END
              GO   TO   INIT-EXT
     END-START.
*    欠品データ初期ＲＥＡＤ
     PERFORM  KMKEPNF-RD-SEC.
*    センターＣＤ退避
     MOVE     KPN-F07        TO   WK-CENTCD.
*    倉庫退避
     MOVE     KPN-F48        TO   WK-SOKO.
*    部門退避
     MOVE     KPN-A40        TO   WK-BUMON.
*    商品退避
     MOVE     KPN-A14        TO   WK-SYOHIN.
     MOVE     KPN-A15        TO   WK-HINM1.
     MOVE     KPN-A16        TO   WK-HINM2.
*    ＪＡＮＣＤ退避
     MOVE     KPN-A13        TO   WK-JANCD.
*    取引先退避
     MOVE     PARA-TORICD    TO   WK-TORIHIKI.
     MOVE     1              TO   TATE.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　欠品データ読込み　　　　　　  構造_            *
*============================================================*
 KMKEPNF-RD-SEC       SECTION.
     MOVE     "KMKEPNF-RD-SEC"   TO   S-NAME.
*    欠品データ読込み
     READ     KMKEPNF  NEXT  AT  END
*****         IF READ-CNT > ZERO
*****            PERFORM   SYUKEI-WT-SEC
*****         END-IF
              MOVE     "END"      TO       FLG-END
              GO                  TO       KMKEPNF-RD-EXIT
     END-READ.
 READ010.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  KPN-F46
     AND      PARA-JTIME  =  KPN-F47
     AND      PARA-TORICD =  KPN-F01
              CONTINUE
     ELSE
              MOVE     "END"      TO   FLG-END
              GO                  TO   KMKEPNF-RD-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                  TO   READ020
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =    KPN-F48
              GO                  TO   READ020
     ELSE
              GO                  TO   KMKEPNF-RD-SEC
     END-IF.
 READ020.
*    センターＣＤ範囲チェック
     IF       KPN-F07   <    PARA-CNTCD1   OR
              KPN-F07   >    PARA-CNTCD2
              GO                  TO   KMKEPNF-RD-SEC
     END-IF.
*    対象データ件数カウント
     ADD      1                   TO   READ-CNT.
*
 KMKEPNF-RD-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    ブレイクチェック（センターＣＤ）
*****DISPLAY "WK-SOKO    = "  WK-SOKO    UPON CONS.
*****DISPLAY "WK-CENTCD  = "  WK-CENTCD  UPON CONS.
*****DISPLAY "KPN-F48    = "  KPN-F48    UPON CONS.
*****DISPLAY "KPN-F07    = "  KPN-F07    UPON CONS.
     IF       WK-SOKO      NOT =  KPN-F48
     OR       WK-CENTCD    NOT =  KPN-F07
              MOVE  WK-BUMON     TO  PRT-BUMON(TATE)
              MOVE  WK-HINM1     TO  PRT-HINM(TATE)
              MOVE  WK-HINM2     TO  PRT-KINM(TATE)
              MOVE  WK-SYOHIN    TO  PRT-SYOCD(TATE)
              MOVE  WK-JANCD     TO  PRT-JANCD(TATE)
              MOVE  WK-JYUTYU    TO  PRT-JYUTYU(TATE)
              MOVE  WK-SYUKA     TO  PRT-SYUKA(TATE)
              COMPUTE   PRT-KEPPIN(TATE)    =
                        WK-JYUTYU      -    WK-SYUKA
              PERFORM            SYUKEI-WT-SEC
              MOVE  KPN-F48      TO  WK-SOKO
              MOVE  KPN-F07      TO  WK-CENTCD
              MOVE  KPN-A40      TO  WK-BUMON
              MOVE  KPN-A15      TO  WK-HINM1
              MOVE  KPN-A16      TO  WK-HINM2
              MOVE  KPN-A14      TO  WK-SYOHIN
              MOVE  KPN-A13      TO  WK-JANCD
              MOVE  1            TO  TATE
              MOVE  ZERO         TO  WK-JYUTYU WK-SYUKA
     ELSE
*    同頁内で部門が２０アイテム以上の時，次頁へ改頁
              IF    TATE  >  15
                    MOVE  WK-BUMON     TO  PRT-BUMON(TATE)
                    MOVE  WK-HINM1     TO  PRT-HINM(TATE)
                    MOVE  WK-HINM2     TO  PRT-KINM(TATE)
                    MOVE  WK-SYOHIN    TO  PRT-SYOCD(TATE)
                    MOVE  WK-JANCD     TO  PRT-JANCD(TATE)
                    MOVE  WK-JYUTYU    TO  PRT-JYUTYU(TATE)
                    MOVE  WK-SYUKA     TO  PRT-SYUKA(TATE)
                    COMPUTE   PRT-KEPPIN(TATE)    =
                              WK-JYUTYU      -    WK-SYUKA
                    PERFORM          SYUKEI-WT-SEC
                    MOVE  KPN-F48    TO  WK-SOKO
                    MOVE  KPN-F07    TO  WK-CENTCD
                    MOVE  KPN-A40    TO  WK-BUMON
                    MOVE  KPN-A15    TO  WK-HINM1
                    MOVE  KPN-A16    TO  WK-HINM2
                    MOVE  KPN-A14    TO  WK-SYOHIN
                    MOVE  KPN-A13    TO  WK-JANCD
                    MOVE  1          TO  TATE
                    MOVE  ZERO       TO  WK-JYUTYU WK-SYUKA
              END-IF
     END-IF.
*    項目セット処理へ
     PERFORM  DATA-SET-SEC.
*    欠品データ読込み
     PERFORM  KMKEPNF-RD-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                 SECTION.
     IF  SYUKEI-CNT     >=   1
*    項目セット
*        部門
         MOVE      WK-BUMON       TO   PRT-BUMON(TATE)
*        品名
         MOVE      WK-HINM1       TO   PRT-HINM(TATE)
         MOVE      WK-HINM2       TO   PRT-KINM(TATE)
*        商品ＣＤ
         MOVE      WK-SYOHIN      TO   PRT-SYOCD(TATE)
*        ＪＡＮＣＤ
         MOVE      WK-JANCD       TO   PRT-JANCD(TATE)
*        受注数
         MOVE      WK-JYUTYU      TO   PRT-JYUTYU(TATE)
*        出荷数
         MOVE      WK-SYUKA       TO   PRT-SYUKA(TATE)
*        欠品数
         COMPUTE   PRT-KEPPIN(TATE)    =
                   WK-JYUTYU      -    WK-SYUKA
         PERFORM             SYUKEI-WT-SEC
     END-IF.
*
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE               KMKEPNF  HTENMS  PRTF.
*    終了メッセージ
     DISPLAY "************************" UPON  CONS.
     DISPLAY "*    欠品報告書        *" UPON  CONS.
     DISPLAY "*  ｼｭﾂﾘｮｸ ｹﾝｽｳ = " PAGE-CNT "  *"  UPON  CONS.
     DISPLAY "************************" UPON  CONS.
*
 END-EXT.
     EXIT.
*============================================================*
*    欠品情報報告書出力処理                構造_2.1         *
*============================================================*
 SYUKEI-WT-SEC         SECTION.
     MOVE     "SYUKEI-WT-SEC"     TO   S-NAME.
*    ページ　カウントアップ
     ADD      1         TO        PAGE-CNT.
*****MOVE    PAGE-CNT   TO        PRT-HDPAGE.
*    システム日付セット
     MOVE    WRK-DATE-YYYY   TO   PRT-SYSYY.
     MOVE    WRK-DATE-MM     TO   PRT-SYSMM.
     MOVE    WRK-DATE-DD     TO   PRT-SYSDD.
*ヘッダ
     MOVE    WK-TORIHIKI     TO   TEN-F52.
     MOVE    WK-CENTCD       TO   TEN-F011.
     READ    HTENMS
         INVALID   KEY
             MOVE   SPACE    TO  PRT-CENTNM
         NOT  INVALID  KEY
             MOVE   TEN-F02  TO  PRT-CENTNM
     END-READ.
*    受注月日
     MOVE    PARA-JDATE(5:2)          TO   PRT-JYUMM.
     MOVE    PARA-JDATE(7:2)          TO   PRT-JYUDD.
*    仕入先ＣＤ
*****MOVE    100403                   TO   PRT-TOKCD.
     MOVE    WK-TORIHIKI              TO   PRT-TOKCD.
*    仕入先名
     MOVE    NC"（株）サカタのタネ"   TO   PRT-TOKNM.
*
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY91151"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    欠品情報報告書出力
 SYUKEI010.
     WRITE    PRT-FSY91151.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FSY91151.
*
 SYUKEI-WT-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO   S-NAME.
**** DISPLAY "TATE = " TATE  UPON CONS.
*
*    明細
*    DISPLAY "WK-SYOHIN = " WK-SYOHIN  UPON CONS.
*    DISPLAY "KPN-F12   = " KPN-F12    UPON CONS.
*    DISPLAY "WK-BUMON  = " WK-BUMON   UPON CONS.
*    DISPLAY "KPN-F25   = " KPN-F25    UPON CONS.
     IF       WK-BUMON       NOT =     KPN-A40   OR
              WK-SYOHIN      NOT =     KPN-A14
*    項目セット
*        部門
         MOVE      WK-BUMON       TO   PRT-BUMON(TATE)
*        品名
         MOVE      WK-HINM1       TO   PRT-HINM(TATE)
         MOVE      WK-HINM2       TO   PRT-KINM(TATE)
*        商品ＣＤ
         MOVE      WK-SYOHIN      TO   PRT-SYOCD(TATE)
*        ＪＡＮＣＤ
         MOVE      WK-JANCD       TO   PRT-JANCD(TATE)
*        受注数
         MOVE      WK-JYUTYU      TO   PRT-JYUTYU(TATE)
*        出荷数
         MOVE      WK-SYUKA       TO   PRT-SYUKA(TATE)
*        欠品数
         COMPUTE   PRT-KEPPIN(TATE)    =
                   WK-JYUTYU      -    WK-SYUKA
         MOVE      KPN-A40        TO   WK-BUMON
         MOVE      KPN-A15        TO   WK-HINM1
         MOVE      KPN-A16        TO   WK-HINM2
         MOVE      KPN-A14        TO   WK-SYOHIN
         MOVE      KPN-A13        TO   WK-JANCD
         MOVE      KPN-F50        TO   WK-JYUTYU
         MOVE      KPN-F15        TO   WK-SYUKA
         ADD       1              TO   TATE
*        同頁内で部門が１５アイテム以上の時，次頁へ改頁
         IF        TATE      >    15
                   PERFORM             SYUKEI-WT-SEC
                   MOVE      KPN-F48   TO   WK-SOKO
                   MOVE      KPN-F07   TO   WK-CENTCD
                   MOVE      1         TO   SYUKEI-CNT
                   MOVE      1         TO   TATE
         END-IF
     ELSE
*        受注数、出荷数加算
         ADD       KPN-F50        TO   WK-JYUTYU
         ADD       KPN-F15        TO   WK-SYUKA
         ADD       1              TO   SYUKEI-CNT
     END-IF.
*
 DATA-SET-EXIT.
     EXIT.

```
