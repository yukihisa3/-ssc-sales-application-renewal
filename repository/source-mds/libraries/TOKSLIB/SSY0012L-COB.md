# SSY0012L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0012L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    サブシステム　　：　出荷管理システム                      *
*    業務名　　　　　：　オンラインデータ発注集計　　　　　　　*
*    モジュール名　　：　発注集計表発行　　　　　　　　　　　　*
*    作成日／更新日　：　99/09/16                              *
*    作成者／更新者　：　ＮＡＶ萩原                            *
*    更新日／更新者　：                                        *
*                                                              *
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SSY0012L.
 AUTHOR.                 HAGIWARA.
 DATE-WRITTEN.           99.09.16.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-STATUS.
*----<< 発注集計プリントファイル >>--*
     SELECT   HACYUPRT  ASSIGN    TO        DA-01-S-HACYUPRT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    HAC-STATUS.
*----<< 帳票ファイル >>-*
     SELECT  PRTFILE   ASSIGN     TO        GS-XU04LP
                       DESTINATION          "PRT"
                       FORMAT               PRT-FORM
                       GROUP                PRT-GRP
                       PROCESSING           PRT-PROC
                       UNIT CONTROL         PRT-CTL
                       FILE STATUS          PRT-STATUS.
*
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 取引先マスタ >>-*
 FD  HTOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*----<< 店舗マスタ >>-*
 FD  HTENMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTENMS    OF   XFDLIB    JOINING   TEN  AS   PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS   PREFIX.
*----<< 発注集計表プリントファイル >>-*
 FD  HACYUPRT           BLOCK     CONTAINS   27  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HACYUPRT  OF   XFDLIB    JOINING   HAC  AS   PREFIX.
*----<< 帳票ファイル >>-*
 FD  PRTFILE.
     COPY     FSY00121  OF   XMDLIB.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  TOK-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  HAC-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
     03  SOK-STATUS               PIC  X(02).
*    ＦＯＲＭ制御領域（帳票）
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
     03  FLG-DATA                 PIC  9(01)  VALUE  ZERO.
*    退避エリア
 01  SAV-AREA.
     03  WK-TOKCD                 PIC  9(08)  VALUE  ZERO.
*****03  WK-SYUKKA                PIC  9(02)  VALUE  ZERO.
     03  WK-SYUKKA                PIC  X(02)  VALUE  ZERO.
     03  WK-NOUNYU-YMD            PIC  9(08)  VALUE  ZERO.
     03  WK-SYOCD                 PIC  X(13)  VALUE  ZERO.
     03  WK-TENCD                 PIC  9(03)  VALUE  ZERO.
     03  WK-TOKUI                 PIC  9(08)  VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  YOKO                     PIC  9(03)  VALUE  ZERO.
     03  WK-GKETA                 PIC  9(03)  VALUE  ZERO.
     03  WK-MAXGP                 PIC  9(02)  VALUE  ZERO.
     03  IA                       PIC  9(03)  VALUE  ZERO.
     03  IB                       PIC  9(03)  VALUE  ZERO.
     03  IX                       PIC  9(03)  VALUE  ZERO.
     03  IY                       PIC  9(03)  VALUE  ZERO.
     03  IZ                       PIC  9(03)  VALUE  ZERO.
     03  WK-TENPO-SET             PIC  9(03)  VALUE  ZERO.
     03  CHK-FLG                  PIC  9(01)  VALUE  ZERO.
     03  KIN-FLG                  PIC  9(01)  VALUE  ZERO.
     03  SYURYO-FLG               PIC  9(01)  VALUE  ZERO.
     03  SYORI-FLG                PIC  9(01)  VALUE  ZERO.
     03  ERR-SW                   PIC  9(01)  VALUE  ZERO.
*    原価，売価
 01  WK-KINGAKU.
     03  WK-GENKA                 PIC S9(11)  VALUE  ZERO.
     03  WK-BAIKA                 PIC S9(11)  VALUE  ZERO.
     03  WK-BAIGEN                PIC  9(09)  VALUE  ZERO.
     03  WK-NEIRE                 PIC  9(03)  VALUE  ZERO.
     03  WK-BAI                   PIC  9(09)V9999 VALUE  ZERO.
*    日付変換
 01  WK-HEN-DATE                  PIC  9(08)  VALUE  ZERO.
 01  WK-DATE.
     03  WK-DATE01                PIC  9(04)  VALUE  ZERO.
     03  WK-DATE02                PIC  9(02)  VALUE  ZERO.
     03  WK-DATE03                PIC  9(02)  VALUE  ZERO.
*    ルート毎店舗情報ワークエリア
 01  WK-TENPO.
     03  TENPO                    OCCURS      225.
         05  TENCD                PIC  9(03)  VALUE  ZERO.
         05  TENMEI               PIC  N(03)  VALUE  SPACE.
*    プリント情報格納エリア
 01  WK-PRINT.
     03  JYOHOU1                  OCCURS      20.
         05  PRT-SKTCD            PIC  X(08).
         05  PRT-KEICD            PIC  X(13).
         05  PRT-SYOCD1           PIC  X(15).
         05  PRT-SYOCD2           PIC  X(15).
         05  PRT-KIKAKU           PIC  X(06).
         05  PRT-SIZE             PIC  X(06).
         05  PRT-IRISU            PIC  9(04).
         05  PRT-GENKA            PIC  9(05)V9.
         05  PRT-BAIKA            PIC  9(05).
         05  JYOHOU2              OCCURS      249.
             07  PRT-TENSU        PIC  9(08).
 01  WK-GOKEI-AREA.
     03  PRT-G                    OCCURS      5.
         05  PRT-GOKEI            PIC S9(11).
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)  VALUE  " *** ".
     03  S-NAME                   PIC  X(30).
*    システム日付
 01  WRK-SYSTEM.
     03  SYS-DATE                 PIC  9(06)  VALUE  ZERO.
     03  WRK-DATE.
         05  WK-YY                PIC  9(04)  VALUE  ZERO.
         05  WK-MM                PIC  9(02)  VALUE  ZERO.
         05  WK-DD                PIC  9(02)  VALUE  ZERO.
     03  WRK-TIME.
         05  WK-HH                PIC  9(02)  VALUE  ZERO.
         05  WK-MN                PIC  9(02)  VALUE  ZERO.
         05  WK-SS                PIC  9(02)  VALUE  ZERO.
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SSY0012L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(15) VALUE
              NC"　『発注集計表　作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　対象データがありません".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                  PIC X(01).
 01  LINK-IN-YMD6                 PIC 9(06).
 01  LINK-IN-YMD8                 PIC 9(08).
 01  LINK-OUT-RET                 PIC X(01).
 01  LINK-OUT-YMD                 PIC 9(08).
*
*============================================================*
 PROCEDURE                        DIVISION.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 FILEERR-SEC1           SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTFILE.
     MOVE      "PRTFILE"        TO   ERR-FL-ID.
     MOVE      PRT-STATUS       TO   ERR-STCD.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　得意先マスタ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTOKMS.
     MOVE      "HTOKMS"         TO   ERR-FL-ID.
     MOVE      TOK-STATUS       TO   ERR-STCD.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　店舗マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTENMS.
     MOVE      "HTENMS"         TO   ERR-FL-ID.
     MOVE      TEN-STATUS       TO   ERR-STCD.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　倉庫マスタ
 FILEERR-SEC4           SECTION.
     USE AFTER EXCEPTION PROCEDURE   ZSOKMS.
     MOVE      "ZSOKMS"         TO   ERR-FL-ID.
     MOVE      SOK-STATUS       TO   ERR-STCD.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　発注集計表プリントファイル
 FILEERR-SEC5           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HACYUPRT.
     MOVE      "HACYUPRT"       TO   ERR-FL-ID.
     MOVE      HAC-STATUS       TO   ERR-STCD.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造_0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
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
     MOVE     "INIT-SEC"          TO       SEC-NAME.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE.
*
     ACCEPT   WRK-TIME            FROM     TIME.
     DISPLAY "*** SSY0012L START " WK-YY "." WK-MM "." WK-DD " "
                  WK-HH ":" WK-MN ":" WK-SS " ***" UPON CONS.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT     HTOKMS    ZSOKMS
                        HACYUPRT.
     OPEN     OUTPUT    PRTFILE.
*
*****PERFORM  DSP-SEC.
     IF       FLG-END = "END"
              GO         TO       INIT-EXIT
     END-IF.
*    プリントエリア初期化
     MOVE     SPACE      TO       FSY00121.
     MOVE     SPACE      TO       WK-PRINT.
     INITIALIZE                   WK-PRINT.
     INITIALIZE                   WK-GOKEI-AREA.
     MOVE     ZERO       TO       WK-KINGAKU.
*    発注集計プリントＦ初期ＲＥＡＤ
     PERFORM  HACYU-RD-SEC.
     IF       FLG-END = "END"
              GO         TO       INIT-EXIT
     END-IF.
*    得意先コード
     MOVE     HAC-F01    TO       WK-TOKCD.
*    出荷場所
     MOVE     HAC-F02    TO       WK-SYUKKA.
*    納入日
     MOVE     HAC-F03    TO       WK-NOUNYU-YMD.
*    量販店商品コード
     MOVE     HAC-F04    TO       WK-SYOCD.
     MOVE     1          TO       TATE.
     PERFORM  TENPO-SET-SEC.
*
 INIT-EXIT.
     EXIT.
*============================================================*
*    発注集計表プリントファイル読込み      構造_            *
*============================================================*
 HACYU-RD-SEC           SECTION.
     MOVE     "HACYU-RD-SEC"      TO       SEC-NAME.
*    発注プリントＦ読込み
     READ     HACYUPRT
         AT END
              MOVE      1    TO        KIN-FLG
              IF   FLG-DATA       =    1
                   PERFORM   DATA-SET-SEC
              END-IF
              MOVE     "END"      TO        FLG-END
              GO   TO   HACYU-RD-EXIT
     END-READ.
     MOVE     1             TO    FLG-DATA.
*
 HACYU-RD-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造_2.0         *
*============================================================*
 MAIN-SEC               SECTION.
     MOVE     "MAIN-SEC"          TO       SEC-NAME.
*    ブレイクチェック（ブレイク時　各ワーク初期化）
*    出荷場所、又は納品日ブレイクチェック　
     IF     ( HAC-F02   NOT =     WK-SYUKKA )
         OR ( HAC-F03   NOT =     WK-NOUNYU-YMD )
              MOVE        1         TO  KIN-FLG
              PERFORM     DATA-SET-SEC
              MOVE        ZERO      TO  KIN-FLG
              MOVE        SPACE     TO  WK-PRINT
              INITIALIZE                WK-PRINT
              MOVE        HAC-F01   TO  WK-TOKCD
              MOVE        HAC-F02   TO  WK-SYUKKA
              MOVE        HAC-F03   TO  WK-NOUNYU-YMD
              MOVE        ZERO      TO  WK-KINGAKU
              MOVE        HAC-F04   TO  WK-SYOCD
              MOVE        1         TO  TATE
     END-IF.
*    ブレイクチェック（商品コード）
     IF       WK-SYOCD       NOT =     HAC-F04
              ADD       1         TO   TATE
              MOVE      HAC-F04   TO   WK-SYOCD
     END-IF.
*    縦添字＞＝５の時プリント処理へ
     IF       TATE      >=   6
              PERFORM   DATA-SET-SEC
              MOVE      SPACE     TO    FSY00121
              MOVE      SPACE     TO    WK-PRINT
              INITIALIZE                WK-PRINT
              MOVE      1         TO    TATE
     END-IF.
*    明細情報格納
     MOVE     HAC-F06             TO   WK-TENCD.
     PERFORM  TENPO-CHK-SEC.
     IF       CHK-FLG   =    0
              GO   TO        MAIN-900
     END-IF.
*
     ADD      HAC-F15             TO   PRT-TENSU(TATE YOKO).
*
     MOVE     HAC-F05             TO   PRT-SKTCD (TATE).
     MOVE     HAC-F04             TO   PRT-KEICD (TATE).
     MOVE     HAC-F101            TO   PRT-SYOCD1(TATE).
     MOVE     HAC-F102            TO   PRT-SYOCD2(TATE).
     MOVE     HAC-F07             TO   PRT-KIKAKU(TATE).
     MOVE     HAC-F08             TO   PRT-SIZE  (TATE).
     MOVE     HAC-F09             TO   PRT-IRISU (TATE).
     MOVE     HAC-F11             TO   PRT-GENKA (TATE).
     MOVE     HAC-F12             TO   PRT-BAIKA (TATE).
*    数量合計へ加算
     ADD      HAC-F15             TO   PRT-GOKEI(TATE).
*    原価金額
     ADD      HAC-F13             TO   WK-GENKA.
*    売価金額
     ADD      HAC-F14             TO   WK-BAIKA.
*
 MAIN-900.
*    発注集計プリントＦ読込み
     PERFORM  HACYU-RD-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造_3.0         *
*============================================================*
 END-SEC                SECTION.
     MOVE     "END-SEC"           TO       SEC-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE              HACYUPRT  HTOKMS
                        ZSOKMS    PRTFILE.
*    終了メッセージ
     DISPLAY "*** ﾊｯﾁｭｳｼｭｳｹｲﾋｮｳ ｼｭﾂﾘｮｸ ﾏｲｽｳ = " PAGE-CNT " ***"
              UPON CONS.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WRK-DATE.
     ACCEPT   WRK-TIME            FROM     TIME.
     DISPLAY "*** SSY0012L END   " WK-YY "." WK-MM "." WK-DD " "
                  WK-HH ":" WK-MN ":" WK-SS " ***" UPON CONS.
*
 END-EXT.
     EXIT.
*============================================================*
*    店舗セット処理                        構造_1.1         *
*============================================================*
 TENPO-SET-SEC          SECTION.
     MOVE     "TENPO-SET-SEC"     TO       SEC-NAME.
*    店舗マスタＯＰＥＮ
     OPEN     INPUT     HTENMS.
*    店舗格納ワーク初期化
     MOVE     ZERO           TO   IX.
     MOVE     SPACE          TO   WK-TENPO.
     INITIALIZE                   WK-TENPO.
*    店舗マスタスタート
     MOVE     WK-TOKCD       TO   TEN-F52.
     MOVE     ZERO           TO   TEN-F011.
     START    HTENMS    KEY  IS   >=   TEN-F52   TEN-F011
         INVALID
              DISPLAY NC"店舗マスタに対象店舗が存在しません。"
                      "ﾄﾘﾋｷｻｷｺｰﾄﾞ = " TEN-F52   UPON    CONS
              STOP RUN
         NOT  INVALID
              MOVE      TEN-F52   TO   WK-TOKUI
     END-START.
*    店舗マスタのＳＥＱ．読み
 RUTO010.
     READ     HTENMS    NEXT      AT   END
              GO        TO        RUTO020
     END-READ.
*    ブレイクチェック
     IF       TEN-F52   NOT =     WK-TOKUI
              GO        TO        RUTO020
     ELSE
              ADD       1         TO        IX
              MOVE      TEN-F011  TO        TENCD(IX)
              MOVE      TEN-F03   TO        TENMEI(IX)
              GO        TO        RUTO010
     END-IF.
*
 RUTO020.
*    合計用エリア作成
     IF      IX         >         ZERO
             MOVE       IX        TO        WK-GKETA
             IF         WK-GKETA  <=        83
                        MOVE      1         TO   WK-MAXGP
             ELSE
                   IF   IX        <=        166
                        MOVE      2         TO   WK-MAXGP
                   ELSE
                        MOVE      3         TO   WK-MAXGP
                   END-IF
             END-IF
     END-IF.
*
     CLOSE         HTENMS.
*
 TENPO-SET-EXIT.
     EXIT.
*============================================================*
*    店舗格納位置検索処理                  構造_1.1         *
*============================================================*
 TENPO-CHK-SEC          SECTION.
     MOVE     "TENPO-CHK-SEC"     TO       SEC-NAME.
*    店舗格納位置検索
     MOVE     ZERO      TO        CHK-FLG.
     PERFORM  VARYING   IY   FROM      1    BY   1
              UNTIL     IY   >    166  OR   CHK-FLG   =    1
         IF   TENCD(IY)      =    WK-TENCD
              MOVE      IY   TO   YOKO
              MOVE      1    TO   CHK-FLG
         END-IF
     END-PERFORM.
*
 TENPO-CHK-EXIT.
     EXIT.
*============================================================*
*    配送発注集計表出力処理                構造_2.1         *
*============================================================*
 PRINT-SEC              SECTION.
     MOVE     "PRINT-SEC"         TO       SEC-NAME.
*    ページ　カウントアップ
     ADD      1         TO        PAGE-CNT.
     MOVE     PAGE-CNT  TO        HDPAGE.
*    システム日付セット
     MOVE     WRK-DATE       TO   HDDATE.
*    量販店名の取得
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
         INVALID
              MOVE      ALL NC"＊"     TO   RYOMEI
         NOT INVALID
              MOVE      TOK-F03        TO   RYOMEI
     END-READ.
*    倉庫名（納入場所）の取得
     MOVE     WK-SYUKKA      TO   SOK-F01.
     READ     ZSOKMS
         INVALID
              MOVE      ALL NC"＊"     TO   NOUNYU
         NOT INVALID
              MOVE      SOK-F02        TO   NOUNYU
     END-READ.
*    オンライン日付セット
     MOVE     WRK-DATE       TO   KIKAKU.
*    納入日転送
     MOVE     WK-NOUNYU-YMD  TO   WK-HEN-DATE.
     MOVE     WK-HEN-DATE    TO   WK-DATE.
     MOVE     WK-DATE01      TO   NOUNEN.
     MOVE     WK-DATE02      TO   NOUTUK.
     MOVE     WK-DATE03      TO   NOUHI.
*    原価，売価転送
     IF       KIN-FLG   =    1
         MOVE      WK-GENKA       TO        GENKEI
         MOVE      WK-BAIKA       TO        BAIKEI
         COMPUTE   WK-BAIGEN =    WK-BAIKA  -    WK-GENKA
         IF   WK-BAIKA  =    ZERO
              MOVE      ZERO           TO   WK-BAI
         ELSE
              COMPUTE   WK-BAI    =    WK-BAIGEN /    WK-BAIKA
         END-IF
         COMPUTE   WK-NEIRE  =    WK-BAI    *    100
         MOVE      WK-NEIRE       TO        NEBIKI
     END-IF.
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY00121"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    発注集計表出力
     WRITE    FSY00121.
*    プリントエリア初期化
     MOVE     SPACE               TO   FSY00121.
*
 PRINT-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO       SEC-NAME.
*
     MOVE    ZERO              TO   SYURYO-FLG  SYORI-FLG.
*    アイテム１～５の時で店舗が１～８３店舗まで（１頁）
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
*******      IF      PRT-SKTCD (IX)  =  SPACE
*                    MOVE   1   TO      SYORI-FLG
*                    GO         TO      DATA010
*******      END-IF
             MOVE    ZERO                  TO       SYURYO-FLG
             PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 83
                                               OR SYURYO-FLG = 1
                     IF   IY  =  1
                          PERFORM JYOHO-SET-SEC
                     END-IF
                     EVALUATE   IY
                         WHEN   1   THRU   17
                                MOVE   1   TO   WK-TENPO-SET
                                MOVE   IY  TO   IB
                                PERFORM PRT-010-SEC
                         WHEN   18  THRU   34
                                MOVE   18  TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  17
                                PERFORM PRT-020-SEC
                         WHEN   35  THRU   51
                                MOVE   35  TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  34
                                PERFORM PRT-030-SEC
                         WHEN   52  THRU   68
                                MOVE   52  TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  51
                                PERFORM PRT-040-SEC
                         WHEN   69  THRU   83
                                MOVE   69  TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  68
                                PERFORM PRT-050-SEC
                     END-EVALUATE
             END-PERFORM
     END-PERFORM.
*
 DATA010.
*    合計数量セット
     IF      WK-MAXGP   =    1
         PERFORM   VARYING   IX   FROM  1  BY  1  UNTIL  IX  > 5
             MOVE       PRT-GOKEI(IX)  TO   GOKEI(IX)
         END-PERFORM
     END-IF.
*
     PERFORM PRINT-SEC.
     IF      WK-MAXGP   =    1
             INITIALIZE      WK-GOKEI-AREA
     END-IF.
     IF      SYURYO-FLG    =     1
             GO      TO          DATA-SET-EXIT
     END-IF.
*    アイテム１～５の時で店舗が８４～１６６店舗まで（２頁）
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
*******      IF      PRT-SKTCD (IX)  =  SPACE
*                    MOVE   1   TO      SYORI-FLG
*                    GO         TO      DATA020
*******      END-IF
             MOVE    ZERO                  TO       SYURYO-FLG
             PERFORM VARYING IY FROM 84 BY 1 UNTIL IY > 166
                                                OR SYURYO-FLG = 1
                     IF   IY  =  84
                          PERFORM JYOHO-SET-SEC
                     END-IF
                     EVALUATE   IY
                         WHEN   84  THRU   100
                                MOVE   84  TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  83
                                PERFORM PRT-010-SEC
                         WHEN   101 THRU   117
                                MOVE   101 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  100
                                PERFORM PRT-020-SEC
                         WHEN   118 THRU   134
                                MOVE   118 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  117
                                PERFORM PRT-030-SEC
                         WHEN   135 THRU   151
                                MOVE   135 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  134
                                PERFORM PRT-040-SEC
                         WHEN   152 THRU   166
                                MOVE   152 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  151
                                PERFORM PRT-050-SEC
                     END-EVALUATE
             END-PERFORM
     END-PERFORM.
 DATA020.
*    合計数量セット
     IF      WK-MAXGP   =    2
         PERFORM   VARYING   IX   FROM  1  BY  1  UNTIL  IX  > 5
             MOVE       PRT-GOKEI(IX)  TO   GOKEI(IX)
         END-PERFORM
     END-IF.
*
     PERFORM         PRINT-SEC.
     IF      WK-MAXGP   =    2
             INITIALIZE      WK-GOKEI-AREA
     END-IF.
     IF      SYURYO-FLG    =     1
             GO      TO          DATA-SET-EXIT
     END-IF.
*    アイテム１～５の時で店舗が１６７～２４９店舗まで（１頁）
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
******       IF      PRT-SKTCD (IX)  =  SPACE
*                    MOVE   1   TO      SYORI-FLG
*                    GO         TO      DATA030
******       END-IF
             MOVE    ZERO                  TO       SYURYO-FLG
             PERFORM VARYING IY FROM 167 BY 1 UNTIL IY > 249
                                               OR SYURYO-FLG = 1
                     IF   IY  =  167
                          PERFORM JYOHO-SET-SEC
                     END-IF
                     EVALUATE   IY
                         WHEN   167 THRU   183
                                MOVE   167 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  166
                                PERFORM PRT-010-SEC
                         WHEN   184 THRU   200
                                MOVE   184 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  183
                                PERFORM PRT-020-SEC
                         WHEN   201 THRU   217
                                MOVE   201 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  200
                                PERFORM PRT-030-SEC
                         WHEN   218 THRU   234
                                MOVE   218 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  217
                                PERFORM PRT-040-SEC
                         WHEN   235 THRU   249
                                MOVE   235 TO   WK-TENPO-SET
                                COMPUTE    IB   =   IY  -  234
                                PERFORM PRT-050-SEC
                     END-EVALUATE
             END-PERFORM
     END-PERFORM.
*
 DATA030.
*    合計数量セット
     IF      WK-MAXGP   =    3
         PERFORM   VARYING   IX   FROM  1  BY  1  UNTIL  IX  > 5
             MOVE       PRT-GOKEI(IX)  TO   GOKEI(IX)
         END-PERFORM
     END-IF.
*
     PERFORM PRINT-SEC.
     IF      WK-MAXGP   =    3
             INITIALIZE      WK-GOKEI-AREA
     END-IF.
*
     IF      SYURYO-FLG    =     1
             GO      TO          DATA-SET-EXIT
     END-IF.
*
 DATA-SET-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 PRT-010-SEC             SECTION.
     MOVE     "PRT-010-SEC"     TO       SEC-NAME.
*    店舗コード，店名セット（店舗ＣＤがワークに存在した分転送）
     MOVE    WK-TENPO-SET     TO    IA.
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 17
             IF      TENCD(IA)  =  ZERO
                     COMPUTE IZ = IZ - 1
                     GO             TO    PRT010
             END-IF
             MOVE    TENCD(IA)      TO    TENCD1(IZ)
             MOVE    TENMEI(IA)     TO    TENME1(IZ)
             ADD     1              TO    IA
     END-PERFORM.
 PRT010.
*    項目転送（出力対象店舗のみ転送，但し１７以上の時は終了へ）
     IF      IB  >  IZ
             IF     IZ  <   17
                    MOVE   1  TO    SYURYO-FLG
             END-IF
             GO               TO    PRT-010-EXIT
     END-IF.
     MOVE    PRT-SKTCD(IX)    TO    SKTCD1(IX).
     MOVE    PRT-KEICD(IX)    TO    KEICD1(IX).
     MOVE    PRT-SYOCD1(IX)   TO    SYOCD1(IX).
     MOVE    PRT-SYOCD2(IX)   TO    SYOCD2(IX).
     MOVE    PRT-KIKAKU(IX)   TO    KIKAK1(IX).
     MOVE    PRT-SIZE  (IX)   TO    SIZE1 (IX).
     MOVE    PRT-IRISU (IX)   TO    IRISU1(IX).
     MOVE    PRT-GENKA (IX)   TO    GENKA1(IX).
     MOVE    PRT-BAIKA (IX)   TO    BAIKA1(IX).
*    投入数量転送
     MOVE    PRT-TENSU(IX IY) TO    TONYU1(IX IB).
*
 PRT-010-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 PRT-020-SEC             SECTION.
     MOVE     "PRT-020-SEC"     TO       SEC-NAME.
*    店舗コード，店名セット
     MOVE    WK-TENPO-SET     TO    IA.
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 17
             IF      TENCD(IA)  =  ZERO
                     COMPUTE  IZ  =  IZ  -  1
                     GO             TO    PRT020
             END-IF
             MOVE    TENCD(IA)      TO    TENCD2(IZ)
             MOVE    TENMEI(IA)     TO    TENME2(IZ)
             ADD     1              TO    IA
     END-PERFORM.
 PRT020.
*    項目転送
     IF      IB  >  IZ
             IF     IZ  <   17
                    MOVE   1  TO    SYURYO-FLG
             END-IF
             GO               TO    PRT-020-EXIT
     END-IF.
     MOVE    PRT-SKTCD(IX)    TO    SKTCD2(IX).
     MOVE    PRT-KEICD(IX)    TO    KEICD2(IX).
     MOVE    PRT-SYOCD1(IX)   TO    SYOCD3(IX).
     MOVE    PRT-SYOCD2(IX)   TO    SYOCD4(IX).
     MOVE    PRT-KIKAKU(IX)   TO    KIKAK2(IX).
     MOVE    PRT-SIZE  (IX)   TO    SIZE2 (IX).
     MOVE    PRT-IRISU (IX)   TO    IRISU2(IX).
     MOVE    PRT-GENKA (IX)   TO    GENKA2(IX).
     MOVE    PRT-BAIKA (IX)   TO    BAIKA2(IX).
*
     MOVE    PRT-TENSU(IX IY) TO    TONYU2(IX IB).
*
 PRT-020-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 PRT-030-SEC             SECTION.
     MOVE     "PRT-030-SEC"     TO       SEC-NAME.
*    店舗コード，店名セット
     MOVE    WK-TENPO-SET     TO    IA.
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 17
             IF      TENCD(IA)  =  ZERO
                     COMPUTE  IZ  =  IZ  -  1
                     GO             TO    PRT030
             END-IF
             MOVE    TENCD(IA)      TO    TENCD3(IZ)
             MOVE    TENMEI(IA)     TO    TENME3(IZ)
             ADD     1              TO    IA
     END-PERFORM.
 PRT030.
*    項目転送
     IF      IB  >  IZ
             IF     IZ  <   17
                    MOVE   1  TO    SYURYO-FLG
             END-IF
             GO               TO    PRT-030-EXIT
     END-IF.
     MOVE    PRT-SKTCD(IX)    TO    SKTCD3(IX).
     MOVE    PRT-KEICD(IX)    TO    KEICD3(IX).
     MOVE    PRT-SYOCD1(IX)   TO    SYOCD5(IX).
     MOVE    PRT-SYOCD2(IX)   TO    SYOCD6(IX).
     MOVE    PRT-KIKAKU(IX)   TO    KIKAK3(IX).
     MOVE    PRT-SIZE  (IX)   TO    SIZE3 (IX).
     MOVE    PRT-IRISU (IX)   TO    IRISU3(IX).
     MOVE    PRT-GENKA (IX)   TO    GENKA3(IX).
     MOVE    PRT-BAIKA (IX)   TO    BAIKA3(IX).
*
     MOVE    PRT-TENSU(IX IY) TO    TONYU3(IX IB).
*
 PRT-030-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 PRT-040-SEC             SECTION.
     MOVE     "PRT-040-SEC"     TO       SEC-NAME.
*    店舗コード，店名セット
     MOVE    WK-TENPO-SET     TO    IA.
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 17
             IF      TENCD(IA)  =  ZERO
                     COMPUTE  IZ  =  IZ  -  1
                     GO             TO    PRT040
             END-IF
             MOVE    TENCD(IA)      TO    TENCD4(IZ)
             MOVE    TENMEI(IA)     TO    TENME4(IZ)
             ADD     1              TO    IA
     END-PERFORM.
 PRT040.
*    項目転送
     IF      IB  >  IZ
             IF     IZ  <   17
                    MOVE   1  TO    SYURYO-FLG
             END-IF
             GO               TO    PRT-040-EXIT
     END-IF.
     MOVE    PRT-SKTCD(IX)    TO    SKTCD4(IX).
     MOVE    PRT-KEICD(IX)    TO    KEICD4(IX).
     MOVE    PRT-SYOCD1(IX)   TO    SYOCD7(IX).
     MOVE    PRT-SYOCD2(IX)   TO    SYOCD8(IX).
     MOVE    PRT-KIKAKU(IX)   TO    KIKAK4(IX).
     MOVE    PRT-SIZE  (IX)   TO    SIZE4 (IX).
     MOVE    PRT-IRISU (IX)   TO    IRISU4(IX).
     MOVE    PRT-GENKA (IX)   TO    GENKA4(IX).
     MOVE    PRT-BAIKA (IX)   TO    BAIKA4(IX).
*
     MOVE    PRT-TENSU(IX IY) TO    TONYU4(IX IB).
*
 PRT-040-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造_2.2         *
*============================================================*
 PRT-050-SEC             SECTION.
     MOVE     "PRT-050-SEC"     TO       SEC-NAME.
*    店舗コード，店名セット
     MOVE    WK-TENPO-SET     TO    IA.
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 15
             IF      TENCD(IA)  =  ZERO
                     COMPUTE  IZ  =  IZ  -  1
                     GO             TO    PRT050
             END-IF
             MOVE    TENCD(IA)      TO    TENCD5(IZ)
             MOVE    TENMEI(IA)     TO    TENME5(IZ)
             ADD     1              TO    IA
     END-PERFORM.
 PRT050.
*    項目転送
     IF      IB  >  IZ
             IF     IZ  <   15
                    MOVE   1  TO    SYURYO-FLG
             END-IF
             GO               TO    PRT-050-EXIT
     END-IF.
     MOVE    PRT-SKTCD(IX)    TO    SKTCD5(IX).
     MOVE    PRT-KEICD(IX)    TO    KEICD5(IX).
     MOVE    PRT-SYOCD1(IX)   TO    SYOCD9(IX).
     MOVE    PRT-SYOCD2(IX)   TO    SYOCDA(IX).
     MOVE    PRT-KIKAKU(IX)   TO    KIKAK5(IX).
     MOVE    PRT-SIZE  (IX)   TO    SIZE5 (IX).
     MOVE    PRT-IRISU (IX)   TO    IRISU5(IX).
     MOVE    PRT-GENKA (IX)   TO    GENKA5(IX).
     MOVE    PRT-BAIKA (IX)   TO    BAIKA5(IX).
*
     MOVE    PRT-TENSU(IX IY) TO    TONYU5(IX IB).
*
 PRT-050-EXIT.
     EXIT.
*============================================================*
*    商品情報セット                        構造_2.2         *
*============================================================*
 JYOHO-SET-SEC           SECTION.
     MOVE     "JYOHO-SET-SEC"   TO       SEC-NAME.
*商品コード，商品名，単価，売価，規格，サイズのセット
     PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 5
             MOVE PRT-SKTCD(IX)  TO SKTCD1(IX) SKTCD2(IX)
                                    SKTCD3(IX) SKTCD4(IX)
                                    SKTCD5(IX)
             MOVE PRT-KEICD(IX)  TO KEICD1(IX) KEICD2(IX)
                                    KEICD3(IX) KEICD4(IX)
                                    KEICD5(IX)
             MOVE PRT-SYOCD1(IX) TO SYOCD1(IX) SYOCD3(IX)
                                    SYOCD5(IX) SYOCD7(IX)
                                    SYOCD9(IX)
             MOVE PRT-SYOCD2(IX) TO SYOCD2(IX) SYOCD4(IX)
                                    SYOCD6(IX) SYOCD8(IX)
                                    SYOCDA(IX)
             MOVE PRT-KIKAKU(IX) TO KIKAK1(IX) KIKAK2(IX)
                                    KIKAK3(IX) KIKAK4(IX)
                                    KIKAK5(IX)
             MOVE PRT-SIZE  (IX) TO SIZE1 (IX) SIZE2(IX)
                                    SIZE3 (IX) SIZE4(IX)
                                    SIZE5 (IX)
             MOVE PRT-IRISU (IX) TO IRISU1(IX) IRISU2(IX)
                                    IRISU3(IX) IRISU4(IX)
                                    IRISU5(IX)
             MOVE PRT-GENKA (IX) TO GENKA1(IX) GENKA2(IX)
                                    GENKA3(IX) GENKA4(IX)
                                    GENKA5(IX)
             MOVE PRT-BAIKA (IX) TO BAIKA1(IX) BAIKA2(IX)
                                    BAIKA3(IX) BAIKA4(IX)
                                    BAIKA5(IX)
     END-PERFORM.
*
 JYOHO-SET-EXIT.
     EXIT.

```
