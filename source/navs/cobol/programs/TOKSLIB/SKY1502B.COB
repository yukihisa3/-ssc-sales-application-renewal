****************************************************************
*         SYSTEM･･･コーナンオンライン　システム                *
*        PG-NAME･･･発注集計表（店可変）作成                    *
*          PG-ID･･･ＳＫＹ１５０２Ｂ                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SKY1502B.
 AUTHOR.                M-ONDA.
 DATE-WRITTEN.          92/10/27.
*
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA        IS        YA
                        YB-21     IS        YB-21.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<集計表データ　Ｆ >>**************************************
     SELECT   UHACHUD   ASSIGN    TO        UHACHUD
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   UHAC-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
*                                                                *
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*--------------------------------------------------------------*
*    FILE = 集計表データファイル  RL=200    BF=5      ORG=SF   *
*--------------------------------------------------------------*
 FD  UHACHUD            BLOCK     CONTAINS  5    RECORDS.
 01  UHAC-REC.
     03  UHAC01                   PIC       X(02).
     03  UHAC02.
         05  UHAC02A              PIC       9(04).
         05  UHAC02B              PIC       9(05).
     03  UHAC03                   PIC       X(04).
     03  UHAC04                   PIC       9(01).
     03  UHAC05                   PIC       9(06).
     03  UHAC06                   PIC       9(06).
     03  UHAC07                   PIC       X(08).
     03  UHAC08                   PIC       X(13).
     03  UHAC09                   PIC       9(03)V9.
     03  UHAC10                   PIC       9(04).
     03  UHAC11                   PIC       9(05)V9.
     03  UHAC12                   PIC       9(07)V9(02).
     03  UHAC13                   PIC       9(07).
     03  UHAC14                   PIC       9(06).
     03  UHAC15                   PIC       X(25).
     03  UHAC16                   PIC       X(20).
     03  UHAC17                   PIC       X(13).
     03  UHAC18                   PIC       X(20).
     03  UHAC19                   PIC       9(03).
     03  UHAC20                   PIC       X(02).
     03  FILLER                   PIC       X(32).
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  UHAC-STATUS                  PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYSYMD-R                 REDEFINES SYSYMD.
         05  SYS-YY               PIC       9(02).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** 発注日ワーク
 01  HACHU-HIZUKE.
     03  HACYMD                   PIC       9(06)  VALUE  ZERO.
     03  HACYMD-R                 REDEFINES HACYMD.
         05  HAC-YY               PIC       9(02).
         05  HAC-MM               PIC       9(02).
         05  HAC-DD               PIC       9(02).
*
***** 納品日ワーク
 01  NOUHIN-HIZUKE.
     03  NOUYMD                   PIC       9(06)  VALUE  ZERO.
     03  NOUYMD-R                 REDEFINES NOUYMD.
         05  NOU-YY               PIC       9(02).
         05  NOU-MM               PIC       9(02).
         05  NOU-DD               PIC       9(02).
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  MISE-CNT                     PIC       9(03)  VALUE  ZERO.
*
***** ラインカウンタ計算領域
 01  W-L-CNT                      PIC       9(03)  VALUE  ZERO.
*
***** 店インデックス
 01  IDX-CNT                      PIC       9(03)  VALUE  ZERO.
*
***** 店テーブルワーク
 01  WK-MISE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MISE                     PIC       9(03)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  ")".
***** テーブルワーク
 01  TBL-MISE-AREA.
     03  TBL-MISE                 OCCURS    200.
         05  T-MISE               PIC       9(03).
         05  T-SURYO              PIC       9(05).
*
 01  WL-IDX-AREA.
     03  WK-IXM                   PIC       9(03)  VALUE  ZERO.
     03  WK-IXD                   PIC       9(03)  VALUE  ZERO.
     03  WK-IX                    PIC       9(03)  VALUE  ZERO.
*
 01  WK-NEW-KEY.
     03  NEW-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  NEW-BUMONCD              PIC       X(04)  VALUE  ZERO.
     03  NEW-SYOHINCD             PIC       X(13)  VALUE  SPACE.
     03  NEW-RTCD                 PIC       X(02)  VALUE  ZERO.
*
 01  WK-OLD-KEY.
     03  OLD-NOHINBI              PIC       9(06)  VALUE  ZERO.
     03  OLD-BUMONCD              PIC       X(04)  VALUE  ZERO.
     03  OLD-SYOHINCD             PIC       X(13)  VALUE  SPACE.
     03  OLD-RTCD                 PIC       X(02)  VALUE  ZERO.
*
***** 原価単価保存ワーク
 01  WGENTAN                      PIC       9(07)V99 VALUE ZERO.
*
***** 売価単価保存ワーク
 01  WBAITAN                      PIC       9(07)  VALUE  ZERO.
*
***** 商品名保存ワーク
 01  WSYOHINNM                    PIC       X(25)  VALUE  SPACE.
*
***** 規格保存ワーク
 01  WKIKAKU                      PIC       X(20)  VALUE  SPACE.
*
***** 企業名保存ワーク
 01  WKIGYONM                     PIC       X(20)  VALUE  SPACE.
*
***** 合計計算ワーク
 01  WSURYO                       PIC       9(06)  VALUE  ZERO.
 01  WGENHEI                      PIC       9(07)  VALUE  ZERO.
 01  WBAIHEI                      PIC       9(07)  VALUE  ZERO.
*
***** 合計見出し領域
 01  G-MIDASI      CHARACTER      TYPE      IS     YA.
     03  SYO-MI                   PIC       N(05)  VALUE
       NC"【小　計】".
     03  GOU-MI                   PIC       N(05)  VALUE
       NC"【合　計】".
     03  SOU-MI                   PIC       N(05)  VALUE
       NC"【総合計】".
     03  CYU-MI                   PIC       N(05)  VALUE
       NC"【中　計】".
*
***** 合計集計ワーク
 01  G-WORK.
     03  SYO-WORK.
         05  SYO-SU               PIC       9(06)  VALUE  ZERO.
         05  SYO-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  SYO-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  SYO-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  SYO-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  CYU-WORK.
         05  CYU-SU               PIC       9(06)  VALUE  ZERO.
         05  CYU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  CYU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  CYU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  CYU-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  GOU-WORK.
         05  GOU-SU               PIC       9(06)  VALUE  ZERO.
         05  GOU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  GOU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  GOU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  GOU-BAIHEI           PIC       9(07)  VALUE  ZERO.
     03  SOU-WORK.
         05  SOU-SU               PIC       9(06)  VALUE  ZERO.
         05  SOU-GENKIN           PIC      S9(09)  VALUE  ZERO.
         05  SOU-GENHEI           PIC       9(07)  VALUE  ZERO.
         05  SOU-BAIKIN           PIC      S9(09)  VALUE  ZERO.
         05  SOU-BAIHEI           PIC       9(07)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SKY1502B".
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
***** 見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"発注企業名："
                   CHARACTER      TYPE      IS     YA.
     03  KIGYONM                  PIC       X(20).
     03  FILLER                   PIC       X(15)  VALUE  SPACE.
     03  FILLER                   PIC       N(13)  VALUE
       NC"【　発　注　集　計　表　】"
                   CHARACTER      TYPE      IS     YB-21.
     03  FILLER                   PIC       X(07)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"発注日："
                   CHARACTER      TYPE      IS     YA.
     03  H-YY                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"年"
                   CHARACTER      TYPE      IS     YA.
     03  H-MM                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"月"
                   CHARACTER      TYPE      IS     YA.
     03  H-DD                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"日"
                   CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  S-YY                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  S-MM                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  S-DD                     PIC       Z9.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
*
***** 見出し行２
 01  MIDASHI2      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"部門コード：".
     03  BUMONCD                  PIC       X(04).
*
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"ルート：".
     03  RTCD                     PIC       X(02).
*
     03  FILLER                   PIC       X(30)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"（手書伝票用）".
     03  FILLER                   PIC       X(20)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"納品日：".
     03  N-YY                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"年".
     03  N-MM                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"月".
     03  N-DD                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"日".
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
     03  FILLER                   PIC       X(07)  VALUE  SPACE.
*
***** 見出し行３
 01  MIDASHI3      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"商　品　名".
     03  FILLER                   PIC       X(21)  VALUE  SPACE.
     03  FILLER                   PIC       N(08)  VALUE
       NC"原単価　数量合計".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
      NC"（店）".
*
***** 見出し行４
 01  MIDASHI4      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(15)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"規　格".
     03  FILLER                   PIC       X(25)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"売単価".
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"数量".
     03  FILLER                   PIC       X(10)  VALUE  SPACE.
*
***** 明細行１
 01  MEISAI1.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINCD                 PIC       X(13).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINNM                 PIC       X(25).
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  GENTAN                   PIC       ZZZZZZ9.99.
     03  GENTANR   REDEFINES      GENTAN    PIC    X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SURYOKEI                 PIC       ZZZZZZ.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MISE-TBL                 OCCURS    10.
         05  MISEBAN              PIC       X(05)  VALUE  SPACE.
         05  FILLER               PIC       X(02)  VALUE  SPACE.
*
***** 明細行２
 01  MEISAI2.
     03  FILLER                   PIC       X(15)  VALUE  SPACE.
     03  KIKAKU                   PIC       X(25).
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  BAITAN                   PIC       ZZZZZZZ.
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  SURYO-TBL                OCCURS    10.
         05  SURYO                PIC       ZZZZZ  VALUE  ZERO.
         05  FILLER               PIC       X(02)  VALUE  SPACE.
*
***** 明細行３
 01  MEISAI3.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  PRT-TEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "-".
*
***** 合計行
 01  GOUKEI        CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  GKMIDASI                 PIC       N(05).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"数量合計".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SUKEI                    PIC       ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"原価金額".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENKEI                   PIC       ----,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"平均原価".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENHEI                   PIC       Z,ZZZ,ZZ9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"売価金額".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  BAIKEI                   PIC       ----,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"平均売価".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  BAIHEI                   PIC       Z,ZZZ,ZZ9.
     03  FILLER                   PIC       X(06)  VALUE  SPACE.
*
***** 線
 01  SEN           CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  PRT-SEN                  OCCURS    68.
        05  FILLER               PIC       N(01)  VALUE  NC"─".
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE UHACHUD.
     MOVE     "UHACHUD "          TO        ERR-FL-ID.
     MOVE     UHAC-STATUS         TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SKOU060-START          SECTION.
*
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SKOU060-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     OPEN     INPUT     UHACHUD.
     OPEN     OUTPUT    PRINTF.
*
     ACCEPT   SYSYMD    FROM      DATE.
*
     PERFORM  UHAC-RD-SEC.
     IF       END-FLG   =   "END"
              GO        TO        INIT-EXIT
     END-IF.
*
     MOVE     1         TO        IDX-CNT.
     PERFORM  CLR-SEC   UNTIL     IDX-CNT   >   10.
*
     MOVE     1         TO        IDX-CNT   MISE-CNT.
     MOVE     ZERO      TO        WK-IXM.
     MOVE     ZERO      TO        TBL-MISE-AREA.
     MOVE     UHAC05    TO        HACYMD.
     MOVE     UHAC06    TO        NOUYMD.
     MOVE     UHAC03    TO        OLD-BUMONCD.
     MOVE     UHAC20    TO        OLD-RTCD.
     MOVE     UHAC08    TO        OLD-SYOHINCD.
     MOVE     UHAC12    TO        WGENTAN.
     MOVE     UHAC13    TO        WBAITAN.
     MOVE     UHAC15    TO        WSYOHINNM.
     MOVE     UHAC16    TO        WKIKAKU.
     MOVE     UHAC18    TO        WKIGYONM.
*
     PERFORM  MIDASI-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             集計表データＲＥＡＤ処理      1.1                *
****************************************************************
 UHAC-RD-SEC            SECTION.
*

     MOVE     NEW-NOHINBI    TO   OLD-NOHINBI.
     MOVE     NEW-BUMONCD    TO   OLD-BUMONCD.
     MOVE     NEW-SYOHINCD   TO   OLD-SYOHINCD.
     MOVE     NEW-RTCD       TO   OLD-RTCD.

     READ     UHACHUD
          AT END
              MOVE     "END"      TO        END-FLG
              MOVE     HIGH-VALUE TO        WK-NEW-KEY
          NOT AT END
              MOVE     UHAC06     TO        NEW-NOHINBI
              MOVE     UHAC08     TO        NEW-SYOHINCD
              MOVE     UHAC03     TO        NEW-BUMONCD
              MOVE     UHAC20     TO        NEW-RTCD
     END-READ.
*
*
 UHAC-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE     WKIGYONM  TO        KIGYONM.
     MOVE     HAC-YY    TO        H-YY.
     MOVE     HAC-MM    TO        H-MM.
     MOVE     HAC-DD    TO        H-DD.
     MOVE     SYS-YY    TO        S-YY.
     MOVE     SYS-MM    TO        S-MM.
     MOVE     SYS-DD    TO        S-DD.
     MOVE     OLD-BUMONCD         TO        BUMONCD.
     MOVE     OLD-RTCD            TO        RTCD.
     MOVE     NOU-YY    TO        N-YY.
     MOVE     NOU-MM    TO        N-MM.
     MOVE     NOU-DD    TO        N-DD.
     ADD      1         TO        P-CNT.
     MOVE     P-CNT     TO        PEIJI.
*
     IF       P-CNT     NOT =     1
              MOVE      SPACE     TO        P-REC
              WRITE     P-REC     AFTER     PAGE
     END-IF.
*
     WRITE    P-REC     FROM      MIDASHI1  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI2  AFTER     1.
     WRITE    P-REC     FROM      SEN       AFTER     1.
     WRITE    P-REC     FROM      MIDASHI3  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI4  AFTER     1.
     WRITE    P-REC     FROM      SEN       AFTER     1.
*    MOVE     SPACE     TO        P-REC.
*    WRITE    P-REC     AFTER     1.
*
     MOVE     7         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
*    合計チェック
     IF       UHAC03    NOT =     OLD-BUMONCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              MOVE      UHAC03    TO        OLD-BUMONCD
              MOVE      UHAC20    TO        OLD-RTCD
              MOVE      UHAC06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    中計チェック
     IF       UHAC20    NOT =     OLD-RTCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              MOVE      UHAC03    TO        OLD-BUMONCD
              MOVE      UHAC20    TO        OLD-RTCD
              MOVE      UHAC06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    小計チェック
     IF       UHAC06    NOT =     NOUYMD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              MOVE      UHAC06    TO        NOUYMD
              PERFORM   MIDASI-SEC
     END-IF.
*
*    商品チェック
     IF       UHAC08    NOT =     OLD-SYOHINCD
              PERFORM   S-CHECK-SEC
              MOVE      UHAC08    TO        OLD-SYOHINCD
     END-IF.
*
*    明細行作成
     ADD      1         TO        WK-IXM.
*
     MOVE     UHAC02B   TO        T-MISE(WK-IXM).
     MOVE     UHAC11    TO        T-SURYO(WK-IXM).
     COMPUTE  WSURYO    =         UHAC11    +         WSURYO.
*
     MOVE     IDX-CNT   TO        WK-IXD.
     ADD      1         TO        IDX-CNT.
     ADD      1         TO        MISE-CNT.
*
     PERFORM  UHAC-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             商品コードチェック            2.1                *
****************************************************************
 S-CHECK-SEC            SECTION.
*
     IF        END-FLG  =             "END"
               MOVE     SPACE          TO        UHAC08
     END-IF.
*****IF       UHAC08    NOT =          OLD-SYOHINCD
*
     COMPUTE   SYO-SU    =
                                  WSURYO    +         SYO-SU.
     COMPUTE   SYO-GENKIN  =
                                  WSURYO *  WGENTAN + SYO-GENKIN.
     COMPUTE   SYO-BAIKIN  =
                                  WSURYO *  WBAITAN + SYO-BAIKIN.
*
     MOVE      OLD-SYOHINCD   TO        SYOHINCD.
     MOVE      WSYOHINNM      TO        SYOHINNM.
     MOVE      WKIKAKU        TO        KIKAKU.
     MOVE      WGENTAN        TO        GENTAN.
     MOVE      WBAITAN        TO        BAITAN.
     MOVE      WSURYO         TO        SURYOKEI.
*
     MOVE   ZERO              TO        WK-IXM.
*
     PERFORM   UNTIL     WK-IXM  >  200   OR
                         WK-IXM  >  WK-IXD
*
               MOVE      ZERO             TO   WK-IX
               PERFORM   MEISAI-SEC
                         UNTIL  WK-IX  >  10
*
                  IF     L-CNT     >=        60
                         PERFORM   MIDASI-SEC
                  END-IF
*
                  IF     MEISAI1   NOT =     SPACE
                         WRITE  P-REC  FROM  MEISAI1  AFTER  1
                         WRITE  P-REC  FROM  MEISAI2  AFTER  1
                         ADD    2      TO    L-CNT
                  END-IF
*
     END-PERFORM.
*
     WRITE     P-REC  FROM  MEISAI3  AFTER  1.
     ADD       1              TO        L-CNT.
*
     MOVE      ZERO           TO        TBL-MISE-AREA.
     MOVE      ZERO           TO        WK-IXM.
*
     MOVE      UHAC08         TO        OLD-SYOHINCD.
     MOVE      UHAC12         TO        WGENTAN.
     MOVE      UHAC13         TO        WBAITAN.
     MOVE      UHAC15         TO        WSYOHINNM.
     MOVE      UHAC16         TO        WKIKAKU.
     MOVE      ZERO           TO        WSURYO.
*
     MOVE      1              TO        IDX-CNT.
     PERFORM   CLR-SEC   UNTIL          IDX-CNT   >   10.
*
     MOVE      1              TO        IDX-CNT.
     MOVE      1              TO        MISE-CNT.
*
 S-CHECK-EXIT.
     EXIT.
*
****************************************************************
*             明細行出力処理                2.1.1              *
****************************************************************
 MEISAI-SEC             SECTION.
     ADD      1         TO   WK-IX.
     IF       WK-IX     >    10
              GO   TO   MEISAI-EXIT
     END-IF.
*
     ADD      1         TO   WK-IXM.
     IF       WK-IXM    >    200
              GO   TO   MEISAI-EXIT
     END-IF.
*
     IF       WK-IXM    >     10
              MOVE   SPACE          TO        SYOHINCD
                                              SYOHINNM
                                              KIKAKU
              MOVE   SPACE          TO        GENTANR
              MOVE   ZERO           TO        BAITAN
              MOVE   ZERO           TO        SURYOKEI
     END-IF.
*
     IF       T-MISE(WK-IXM)      =    ZERO
              MOVE      SPACE          TO   MISEBAN(WK-IX)
     ELSE
              MOVE      T-MISE(WK-IXM)      TO   MISE
              MOVE      WK-MISE             TO   MISEBAN(WK-IX)
     END-IF.
     MOVE     T-SURYO(WK-IXM)     TO   SURYO(WK-IX).
*
 MEISAI-EXIT.
     EXIT.
*
***************************************************************
*             テーブルクリア処理            2.2               *
***************************************************************
 CLR-SEC                SECTION.
*
     MOVE     SPACE     TO        MISEBAN(IDX-CNT).
     MOVE     ZERO      TO        SURYO(IDX-CNT).
     ADD      1         TO        IDX-CNT.
*
 CLR-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     IF       WSURYO    NOT =     ZERO
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   CYUKEI-SEC
              PERFORM   GOUKEI-SEC
              PERFORM   SOUKEI-SEC
     END-IF.
*
     CLOSE    UHACHUD   PRINTF.
*
 END-EXIT.
     EXIT.
*
****************************************************************
*             小計行出力処理                3.1                *
****************************************************************
 SYOKEI-SEC             SECTION.
*
     MOVE     SYO-MI         TO        GKMIDASI.
     MOVE     SYO-SU         TO        SUKEI
     MOVE     SYO-GENKIN     TO        GENKEI.
     MOVE     SYO-BAIKIN     TO        BAIKEI.
*
     IF       SYO-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     SYO-GENKIN / SYO-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     SYO-BAIKIN / SYO-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     IF       L-CNT       >= 54
              PERFORM        MIDASI-SEC
     END-IF.
*
     COMPUTE  W-L-CNT     =  54             -    L-CNT.
     WRITE    P-REC          FROM      GOUKEI    AFTER    W-L-CNT.
*
     COMPUTE  CYU-SU      =  SYO-SU         +    CYU-SU.
     COMPUTE  CYU-GENKIN  =  SYO-GENKIN     +    CYU-GENKIN.
     COMPUTE  CYU-BAIKIN  =  SYO-BAIKIN     +    CYU-BAIKIN.
*
     MOVE     ZERO           TO        SYO-SU
                                       SYO-GENKIN
                                       SYO-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 SYOKEI-EXIT.
     EXIT.
*
****************************************************************
*             中計行出力処理                3.2                *
****************************************************************
 CYUKEI-SEC             SECTION.
*
     MOVE     CYU-MI         TO        GKMIDASI.
     MOVE     CYU-SU         TO        SUKEI
     MOVE     CYU-GENKIN     TO        GENKEI.
     MOVE     CYU-BAIKIN     TO        BAIKEI.
*
     IF       CYU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     CYU-GENKIN / CYU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     CYU-BAIKIN / CYU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     2.
*
     COMPUTE  GOU-SU      =  CYU-SU         +    GOU-SU.
     COMPUTE  GOU-GENKIN  =  CYU-GENKIN     +    GOU-GENKIN.
     COMPUTE  GOU-BAIKIN  =  CYU-BAIKIN     +    GOU-BAIKIN.
*
     MOVE     ZERO           TO        CYU-SU
                                       CYU-GENKIN
                                       CYU-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 CYUKEI-EXIT.
     EXIT.
*
****************************************************************
*             合計行出力処理                3.2                *
****************************************************************
 GOUKEI-SEC             SECTION.
*
     MOVE     GOU-MI         TO        GKMIDASI.
     MOVE     GOU-SU         TO        SUKEI
     MOVE     GOU-GENKIN     TO        GENKEI.
     MOVE     GOU-BAIKIN     TO        BAIKEI.
*
     IF       GOU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     GOU-GENKIN / GOU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     GOU-BAIKIN / GOU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     2.
*
     COMPUTE  SOU-SU      =  GOU-SU         +    SOU-SU.
     COMPUTE  SOU-GENKIN  =  GOU-GENKIN     +    SOU-GENKIN.
     COMPUTE  SOU-BAIKIN  =  GOU-BAIKIN     +    SOU-BAIKIN.
*
     MOVE     ZERO           TO        GOU-SU
                                       GOU-GENKIN
                                       GOU-BAIKIN
                                       WGENHEI
                                       WBAIHEI.
*
 GOUKEI-EXIT.
     EXIT.
*
****************************************************************
*             総計行出力処理                3.3                *
****************************************************************
 SOUKEI-SEC             SECTION.
*
     MOVE     SOU-MI         TO        GKMIDASI.
     MOVE     SOU-SU         TO        SUKEI
     MOVE     SOU-GENKIN     TO        GENKEI.
     MOVE     SOU-BAIKIN     TO        BAIKEI.
*
     IF       SOU-SU      =  ZERO
              MOVE           ZERO      TO        GENHEI
                                                 BAIHEI
     ELSE
              COMPUTE        WGENHEI   =     SOU-GENKIN / SOU-SU
              MOVE           WGENHEI   TO        GENHEI
              COMPUTE        WBAIHEI   =     SOU-BAIKIN / SOU-SU
              MOVE           WBAIHEI   TO        BAIHEI
     END-IF.
*
     WRITE    P-REC          FROM      GOUKEI    AFTER     2.
*
 SOUKEI-EXIT.
     EXIT.
*
********************<<  PUROGRAM  END  >>*************************
