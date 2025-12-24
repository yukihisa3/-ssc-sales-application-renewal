****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オフライン出荷　　　　　　　　　　*
*    業務名　　　　　　　：　スポット出荷                      *
*    モジュール名　　　　：　スポットピッキングリスト          *
*    作成日／更新日　　　：　1999/10/20                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　スポットピッキングデータより、    *
*                            スポットピッキングリストを発行    *
*　　　　　　　　　　　　　　する。　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SKY1407L.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/10/20.
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
*****<<スポット発注データ>>*************************************
     SELECT   SPOTWK    ASSIGN    TO        SPOTWK
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SPW-STATUS.
*****<<商品名称マスタ>>*****************************************
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
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
*    FILE = スポット発注データ    RL=200    BF=5      ORG=SF   *
*--------------------------------------------------------------*
 FD  SPOTWK             BLOCK     CONTAINS  5    RECORDS.
     COPY     SPOTWK    OF        XFDLIB
              JOINING   SPW  AS   PREFIX.
*--------------------------------------------------------------*
*    FILE = 商品名称マスタ                                     *
*--------------------------------------------------------------*
 FD  HMEIMS             BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
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
 01  TANA-FLG                     PIC       9(01)  VALUE  ZERO.
 01  HMEIMS-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  SPW-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
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
***** 発注日ワーク
 01  HU-HIZUKE.
     03  HACYMD                   PIC       9(08)  VALUE  ZERO.
     03  HACYMD-R                 REDEFINES HACYMD.
         05  SPW-YY               PIC       9(04).
         05  SPW-MM               PIC       9(02).
         05  SPW-DD               PIC       9(02).
*
***** 納品日ワーク
 01  NOUHIN-HIZUKE.
     03  NOUYMD                   PIC       9(08)  VALUE  ZERO.
     03  NOUYMD-R                 REDEFINES NOUYMD.
         05  NOU-YY               PIC       9(04).
         05  NOU-MM               PIC       9(02).
         05  NOU-DD               PIC       9(02).
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
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
     03  MISE                     PIC       9(05)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  ")".
***** テーブルワーク
 01  TBL-MISE-AREA.
     03  TBL-MISE                 OCCURS    300.
         05  T-MISE               PIC       9(05).
         05  T-SURYO              PIC       9(05).
*
 01  WK-BACHI-AREA.
     03  WK-JDATE                 PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-JTIME                 PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-TORICD                PIC       9(08)  VALUE  ZERO.
*
 01  WL-IDX-AREA.
     03  WK-IXM                   PIC       9(03)  VALUE  ZERO.
     03  WK-IXD                   PIC       9(03)  VALUE  ZERO.
     03  WK-IX                    PIC       9(03)  VALUE  ZERO.
*
 01  WK-NEW-KEY.
     03  NEW-TOKCD                PIC       9(08)  VALUE  ZERO.
     03  NEW-HATDT                PIC       9(08)  VALUE  ZERO.
     03  NEW-NOUDT                PIC       9(08)  VALUE  ZERO.
     03  NEW-TANCD                PIC       X(01)  VALUE  SPACE.
     03  NEW-SYOCD                PIC       X(16)  VALUE  SPACE.
     03  NEW-MEMO1                PIC       9(04)  VALUE  ZERO.
     03  NEW-MEMO2                PIC       9(02)  VALUE  ZERO.
*
 01  WK-OLD-KEY.
     03  OLD-TOKCD                PIC       9(08)  VALUE  ZERO.
     03  OLD-HATDT                PIC       9(08)  VALUE  ZERO.
     03  OLD-NOUDT                PIC       9(08)  VALUE  ZERO.
     03  OLD-TANCD                PIC       X(01)  VALUE  SPACE.
     03  OLD-SYOCD                PIC       X(16)  VALUE  SPACE.
     03  OLD-MEMO1                PIC       9(04)  VALUE  ZERO.
     03  OLD-MEMO2                PIC       9(02)  VALUE  ZERO.
*
***** 商品合計数量
 01  WSURYO                       PIC       9(06)  VALUE  ZERO.
*
***** 原価単価保存ワーク
 01  WGENTAN                      PIC       9(07)V99 VALUE ZERO.
*
***** 売価単価保存ワーク
 01  WBAITAN                      PIC       9(07)  VALUE  ZERO.
*
***** _番保存ワーク
 01  WTANABAN                     PIC       X(06)  VALUE  SPACE.
*
***** 自社商品コード保存ワーク
 01  WJISHOCD                     PIC       X(16)  VALUE  SPACE.
*
***** 商品名保存ワーク
 01  WSYOHINNM                    PIC       X(30)  VALUE  SPACE.
*
***** 企業名保存ワーク
 01  WKIGYONM                     PIC       X(20)  VALUE  SPACE.
***** 商品名保存ワーク（漢字）
 01  WK-SYONM1                    PIC       N(15)  VALUE  SPACE.
 01  WK-SYONM2                    PIC       N(15)  VALUE  SPACE.
***** メモ番号
 01  WMEMO1                       PIC       9(04)  VALUE  ZERO.
 01  WMEMO2                       PIC       9(02)  VALUE  ZERO.
*
***** 合計計算ワーク
 01  WK-GOKEI.
     03  WK-T-GOKEI.
         05 WK-T-ASU              PIC       9(07)  VALUE  ZERO.
         05 WK-T-GSU              PIC       9(07)  VALUE  ZERO.
     03  WK-G-GOKEI.
         05 WK-G-ASU              PIC       9(07)  VALUE  ZERO.
         05 WK-G-GSU              PIC       9(07)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKY1407L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY1407L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SKY1407L".
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG = ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
***** 見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  X(08)  VALUE "SKY1407L".
     03  FILLER                   PIC  X(26)  VALUE  SPACE.
     03  FILLER                   PIC  N(15)  VALUE
       NC"※※　発　注　集　計　表　※※"
                   CHARACTER      TYPE IS     YB-21.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  HD1-TEISEI               PIC  N(07)
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(09)  VALUE  SPACE.
     03  FILLER                   PIC  N(04) VALUE
               NC"処理日："       CHARACTER  TYPE  IS  YA.
     03  HD1-YY                   PIC  9999.
     03  FILLER                   PIC  N(01)  VALUE
       NC"年"
                   CHARACTER      TYPE IS     YA.
     03  HD1-MM                   PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"月"
                   CHARACTER      TYPE IS     YA.
     03  HD1-DD                   PIC  Z9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"日"
                   CHARACTER      TYPE IS     YA.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  HD1-PAGE                 PIC  ZZ9.
     03  FILLER                   PIC  N(01)  VALUE
       NC"頁"
                   CHARACTER      TYPE IS     YA.
*
***** 見出し行３
 01  MIDASHI2.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(06)  VALUE
       NC"発注企業名："
                   CHARACTER      TYPE      IS     YA.
     03  KIGYONM                  PIC       X(20).
     03  FILLER                   PIC       X(72)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"発注日："
                   CHARACTER      TYPE      IS     YA.
     03  H-YY                     PIC       9999.
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
     03  FILLER                   PIC       X(09)  VALUE  SPACE.
*
***** 見出し行４
 01  MIDASHI3      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(105) VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"納品日：".
     03  N-YY                     PIC       9999.
     03  FILLER                   PIC       N(01)  VALUE
       NC"年".
     03  N-MM                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"月".
     03  N-DD                     PIC       Z9.
     03  FILLER                   PIC       N(01)  VALUE
       NC"日".
     03  FILLER                   PIC       X(09)  VALUE  SPACE.
*
***** 見出し行５
 01  MIDASHI4      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"自社商品コード".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"商　品　名".
     03  FILLER                   PIC       X(04)  VALUE  "(ｶﾅ)".
     03  FILLER                   PIC       X(18)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"原単価".
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"数量合計".
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
***** 見出し行６
 01  MIDASHI5      CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(17)  VALUE  SPACE.
     03  FILLER                   PIC       N(03)  VALUE
       NC"規　格".
     03  FILLER                   PIC       X(26)  VALUE  SPACE.
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
***** 明細行７
 01  MIDASHI6      CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       N(02)  VALUE
       NC"_番".
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  FILLER                   PIC       X(07) VALUE "(ﾒﾓNO.)".
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  FILLER                   PIC       N(09)  VALUE
       NC"商　品　名（漢字）".
*
***** 明細行１
 01  MEISAI1.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINCD                 PIC       X(16).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SYOHINNM                 PIC       X(30).
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GENTAN                   PIC       ZZZZZZ9.99.
     03  GENTANR   REDEFINES      GENTAN    PIC    X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  SURYOKEI                 PIC       ZZZZZZ.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MISE-TBL                 OCCURS    10.
         05  MISEBAN              PIC       X(07)  VALUE  SPACE.
*
***** 明細行２
 01  MEISAI2.
     03  FILLER                   PIC       X(48)  VALUE  SPACE.
     03  BAITAN                   PIC       ZZZZZZZ.
     03  FILLER                   PIC       X(11)  VALUE  SPACE.
     03  SURYO-TBL                OCCURS    10.
         05  SURYO                PIC       ZZZZZ  VALUE  ZERO.
         05  FILLER               PIC       X(02)  VALUE  SPACE.
*
***** 明細行４
 01  MEISAI3       CHARACTER      TYPE      IS     YA.
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  PRT-TANA-1               PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  PRT-TANA-2               PIC       X(03).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  PRT-TANA-3               PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  M-MEMO1                  PIC       9(04).
     03  M-MEMO-K                 PIC       X(01).
     03  M-MEMO2                  PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE  ")".
*****03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  M-SYONM1                 PIC       N(15).
     03  M-SYONM2                 PIC       N(15).
*
***** 合計行
 01  GOKEI         CHARACTER      TYPE      IS     YA.
     03  FILLER                   PIC       X(40)  VALUE  SPACE.
     03  TAITORU                  PIC       N(06).
     03  FILLER                   PIC       X(03)  VALUE  " :".
     03  KEI-SURYO                PIC       -,---,--9.
*
***** 線１
 01  SEN1.
     03  PRT-TEN                  OCCURS    136.
         05  FILLER               PIC       X(01)  VALUE  "-".
***** 線２
 01  SEN2          CHARACTER      TYPE      IS     YA.
     03  PRT-TEN                  OCCURS    68.
        05  FILLER               PIC       N(01)  VALUE  NC"─".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SPOTWK.
     MOVE     "SPOTWK "           TO        ERR-FL-ID.
     MOVE     SPW-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HMEIMS.
     MOVE     "HMEIMS  "          TO        ERR-FL-ID.
     MOVE     MEI-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SKY1404-START          SECTION.
*
     MOVE     "SKY1404-START"     TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SKY1404-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SPOTWK  HMEIMS.
     OPEN     OUTPUT    PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     PERFORM  SPW-RD-SEC.
     IF       END-FLG   =   "END"
              GO        TO        INIT-EXIT
     END-IF.
*
     MOVE     1         TO        IDX-CNT.
     PERFORM  CLR-SEC   UNTIL     IDX-CNT   >   10.
*
     MOVE     1            TO        IDX-CNT   MISE-CNT.
     MOVE     ZERO         TO        WK-IXM.
     MOVE     ZERO         TO        TBL-MISE-AREA.
     MOVE     SPW-F05      TO        HACYMD.
     MOVE     SPW-F06      TO        NOUYMD.
*
     MOVE     SPW-F15      TO        WGENTAN.
     MOVE     SPW-F16      TO        WBAITAN.
     MOVE     SPW-F11      TO        WSYOHINNM.
     MOVE     SPW-F17      TO        WKIGYONM.
     MOVE     SPW-F08      TO        WJISHOCD  MEI-F01.
     PERFORM  HMEIMS-READ-SEC.
     IF       HMEIMS-INV-FLG  =  "INV"
              MOVE  ALL NC"＊"  TO   WK-SYONM1
              MOVE  ALL NC"＊"  TO   WK-SYONM2
     ELSE
              MOVE  MEI-F021    TO   WK-SYONM1
              MOVE  MEI-F022    TO   WK-SYONM2
     END-IF.
     MOVE     SPW-F09      TO        WTANABAN.
     MOVE     SPW-F201     TO        WMEMO1.
     MOVE     SPW-F202     TO        WMEMO2.
     MOVE     SPW-F013     TO        OLD-TOKCD.
     MOVE     SPW-F05      TO        OLD-HATDT.
     MOVE     SPW-F06      TO        OLD-NOUDT.
     MOVE     SPW-F09(1:1) TO        OLD-TANCD.
     MOVE     SPW-F08      TO        OLD-SYOCD.
     MOVE     SPW-F201     TO        OLD-MEMO1.
     MOVE     SPW-F202     TO        OLD-MEMO2.
*
     PERFORM  MIDASI-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             集計表データＲＥＡＤ処理      1.1                *
****************************************************************
 SPW-RD-SEC             SECTION.
*
     MOVE    "SPW-RD-SEC"    TO   S-NAME.
     MOVE     NEW-TOKCD      TO   OLD-TOKCD.
     MOVE     NEW-HATDT      TO   OLD-HATDT.
     MOVE     NEW-NOUDT      TO   OLD-NOUDT.
     MOVE     NEW-TANCD      TO   OLD-TANCD.
     MOVE     NEW-SYOCD      TO   OLD-SYOCD.
     MOVE     NEW-MEMO1      TO   OLD-MEMO1.
     MOVE     NEW-MEMO2      TO   OLD-MEMO2.
*
     READ     SPOTWK
          AT END
              MOVE     "END"      TO        END-FLG
              MOVE     HIGH-VALUE TO        WK-NEW-KEY
          NOT AT END
              MOVE     SPW-F013   TO        NEW-TOKCD
              MOVE     SPW-F05    TO        NEW-HATDT
              MOVE     SPW-F06    TO        NEW-NOUDT
              MOVE     SPW-F09(1:1) TO      NEW-TANCD
              MOVE     SPW-F08    TO        NEW-SYOCD
              MOVE     SPW-F201   TO        NEW-MEMO1
              MOVE     SPW-F202   TO        NEW-MEMO2
              ADD      1          TO        CNT-READ
     END-READ.
*
 SPW-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"         TO    S-NAME.
     MOVE     NC"（オフライン）"  TO    HD1-TEISEI.
     MOVE     SYS-YY         TO   HD1-YY.
     MOVE     SYS-MM         TO   HD1-MM.
     MOVE     SYS-DD         TO   HD1-DD.
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD1-PAGE.
*
     MOVE     WKIGYONM       TO   KIGYONM.
     MOVE     SPW-YY         TO   H-YY.
     MOVE     SPW-MM         TO   H-MM.
     MOVE     SPW-DD         TO   H-DD.
     MOVE     NOU-YY         TO   N-YY.
     MOVE     NOU-MM         TO   N-MM.
     MOVE     NOU-DD         TO   N-DD.
*
     IF       P-CNT     NOT =     1
              MOVE      SPACE     TO        P-REC
              WRITE     P-REC     AFTER     PAGE
     END-IF.
*
     WRITE    P-REC     FROM      MIDASHI1  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI2  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI3  AFTER     1.
     WRITE    P-REC     FROM      SEN2      AFTER     1.
     WRITE    P-REC     FROM      MIDASHI6  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI4  AFTER     1.
     WRITE    P-REC     FROM      MIDASHI5  AFTER     1.
     WRITE    P-REC     FROM      SEN2      AFTER     1.
*****MOVE     SPACE     TO        P-REC.
*****WRITE    P-REC     AFTER     1.
*
     MOVE     9         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
 MAIN010.
*    取引先・発注日・納品日
*****DISPLAY "SPW-F013    = " SPW-F013    UPON CONS.
*****DISPLAY "OLD-TOKCD   = " OLD-TOKCD   UPON CONS.
     IF       SPW-F013     NOT =     OLD-TOKCD
     OR       SPW-F05      NOT =     OLD-HATDT
     OR       SPW-F06      NOT =     OLD-NOUDT
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   GOUKEI-SEC
              MOVE      SPW-F013   TO        OLD-TOKCD
              MOVE      SPW-F05    TO        OLD-HATDT
              MOVE      SPW-F06    TO        OLD-NOUDT
              MOVE      SPW-F09(1:1) TO      OLD-TANCD
              MOVE      SPW-F08    TO        OLD-SYOCD
              PERFORM   MIDASI-SEC
     END-IF.
 MAIN020.
*
*    _番上１桁ﾌﾞﾚｲｸ
     IF       SPW-F09(1:1) NOT =     OLD-TANCD
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              MOVE      SPW-F013   TO        OLD-TOKCD
              MOVE      SPW-F05    TO        OLD-HATDT
              MOVE      SPW-F06    TO        OLD-NOUDT
              MOVE      SPW-F09(1:1) TO      OLD-TANCD
              MOVE      SPW-F08    TO        OLD-SYOCD
              MOVE      SPW-F201   TO        OLD-MEMO1
              MOVE      SPW-F202   TO        OLD-MEMO2
              PERFORM   MIDASI-SEC
     END-IF.
 MAIN030.
*
*    商品チェック
     IF       SPW-F08    NOT =     OLD-SYOCD
     OR       SPW-F201   NOT =     OLD-MEMO1
     OR       SPW-F202   NOT =     OLD-MEMO2
              PERFORM   S-CHECK-SEC
              MOVE      SPW-F08    TO        OLD-SYOCD
              MOVE      SPW-F201   TO        OLD-MEMO1
              MOVE      SPW-F202   TO        OLD-MEMO2
     END-IF.
*
*    明細行作成
     ADD      1         TO        WK-IXM.
*
     MOVE     SPW-F02   TO        T-MISE(WK-IXM).
*
     MOVE     SPW-F13   TO        T-SURYO(WK-IXM).
     ADD      SPW-F13   TO        WSURYO.
*
     MOVE     IDX-CNT   TO        WK-IXD.
     ADD      1         TO        IDX-CNT.
     ADD      1         TO        MISE-CNT.
*
     PERFORM  SPW-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             商品コードチェック            2.1                *
****************************************************************
 S-CHECK-SEC            SECTION.
*
     MOVE     "S-CHECK-SEC"            TO        S-NAME.
*
     ADD       1              TO        WK-T-ASU  WK-G-ASU.
     ADD       WSURYO         TO        WK-T-GSU  WK-G-GSU.
*
     MOVE      OLD-SYOCD      TO        SYOHINCD.
     MOVE      WTANABAN(1:1)  TO        PRT-TANA-1.
     MOVE      WTANABAN(2:3)  TO        PRT-TANA-2.
     MOVE      WTANABAN(5:2)  TO        PRT-TANA-3.
     MOVE      WSYOHINNM      TO        SYOHINNM.
     MOVE      WJISHOCD       TO        SYOHINCD.
     MOVE      WGENTAN        TO        GENTAN.
     MOVE      WBAITAN        TO        BAITAN.
     MOVE      WSURYO         TO        SURYOKEI.
     MOVE      WK-SYONM1      TO        M-SYONM1.
     MOVE      WK-SYONM2      TO        M-SYONM2.
     MOVE      WMEMO1         TO        M-MEMO1.
     MOVE      "-"            TO        M-MEMO-K.
     MOVE      WMEMO2         TO        M-MEMO2.
*
     MOVE   ZERO              TO        WK-IXM.
*
     PERFORM   UNTIL     WK-IXM  >  300   OR
                         WK-IXM  >  WK-IXD
*
               MOVE      ZERO             TO   WK-IX
               PERFORM   MEISAI-SEC
                         UNTIL  WK-IX  >  10
*
                  IF     L-CNT     >=        58
                         PERFORM   MIDASI-SEC
                  END-IF
*
                  IF     MEISAI1   NOT =     SPACE
                         IF  TANA-FLG  =  ZERO
                             WRITE P-REC FROM MEISAI3 AFTER 1
                             ADD   1   TO    L-CNT
                         END-IF
                         WRITE  P-REC  FROM  MEISAI1  AFTER  1
                         WRITE  P-REC  FROM  MEISAI2  AFTER  1
                         ADD    2      TO    L-CNT
                  END-IF
*
     END-PERFORM.
*
     WRITE     P-REC  FROM  SEN1  AFTER  1.
     ADD       1              TO        L-CNT.
*
     MOVE      ZERO           TO        TBL-MISE-AREA.
     MOVE      ZERO           TO        WK-IXM.
*
     MOVE      SPW-F15        TO        WGENTAN.
     MOVE      SPW-F16        TO        WBAITAN.
     MOVE      SPW-F11        TO        WSYOHINNM.
     MOVE      SPW-F17        TO        WKIGYONM.
     MOVE      SPW-F08        TO        WJISHOCD  MEI-F01.
     PERFORM  HMEIMS-READ-SEC.
     IF       HMEIMS-INV-FLG  =  "INV"
              MOVE  ALL NC"＊"  TO   WK-SYONM1
              MOVE  ALL NC"＊"  TO   WK-SYONM2
     ELSE
              MOVE  MEI-F021    TO   WK-SYONM1
              MOVE  MEI-F022    TO   WK-SYONM2
     END-IF.
     MOVE      SPW-F09        TO        WTANABAN.
     MOVE      SPW-F201       TO        WMEMO1.
     MOVE      SPW-F202       TO        WMEMO2.
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
*
     MOVE    "MEISAI-SEC"    TO   S-NAME.
     ADD      1         TO   WK-IX.
*
     IF       WK-IX     =    1
              MOVE      ZERO      TO   TANA-FLG
     END-IF.
*
     IF       WK-IX     >    10
              GO   TO   MEISAI-EXIT
     END-IF.
*
     ADD      1         TO   WK-IXM.
     IF       WK-IXM    >    300
              GO   TO   MEISAI-EXIT
     END-IF.
*
     IF       WK-IXM    >     10
              MOVE   SPACE          TO        SYOHINCD
                                              SYOHINNM
                                              PRT-TANA-1
                                              PRT-TANA-2
                                              PRT-TANA-3
                                              GENTANR
              MOVE   ZERO           TO        SURYOKEI
              MOVE   1              TO        TANA-FLG
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
     MOVE    "CLR-SEC"  TO        S-NAME.
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
     MOVE    "END-SEC"  TO        S-NAME.
     IF       WSURYO    NOT =     ZERO
              PERFORM   S-CHECK-SEC
              PERFORM   SYOKEI-SEC
              PERFORM   GOUKEI-SEC
     END-IF.
*
     MOVE      CNT-READ  TO      IN-CNT.
     MOVE      P-CNT     TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE    SPOTWK   PRINTF.
*
 END-EXIT.
     EXIT.
*
****************************************************************
*             小計行出力処理                3.1                *
****************************************************************
 SYOKEI-SEC             SECTION.
*
     MOVE    "SYOKEI-SEC"    TO        S-NAME.
*＜小計＞アイテム合計
     MOVE    NC"アイテム小計" TO   TAITORU.
     MOVE    WK-T-ASU         TO   KEI-SURYO.
     WRITE   P-REC  FROM  GOKEI  AFTER  2.
*＜小計＞発注合計
     MOVE    NC"　　出荷小計" TO   TAITORU.
     MOVE    WK-T-GSU         TO   KEI-SURYO.
     WRITE   P-REC  FROM  GOKEI  AFTER  1.
*
     MOVE    ZERO             TO   WK-T-ASU.
     MOVE    ZERO             TO   WK-T-GSU.
*
 SYOKEI-EXIT.
     EXIT.
*
****************************************************************
*             合計行出力処理                3.2                *
****************************************************************
 GOUKEI-SEC             SECTION.
*
     MOVE    "GOUKEI-SEC"    TO        S-NAME.
*＜合計＞アイテム合計
     MOVE    NC"アイテム合計" TO   TAITORU.
     MOVE    WK-G-ASU         TO   KEI-SURYO.
     WRITE   P-REC  FROM  GOKEI  AFTER  2.
*＜合計＞発注合計
     MOVE    NC"　　出荷合計" TO   TAITORU.
     MOVE    WK-G-GSU         TO   KEI-SURYO.
     WRITE   P-REC  FROM  GOKEI  AFTER  1.
*
     MOVE    ZERO             TO   WK-G-ASU.
     MOVE    ZERO             TO   WK-G-GSU.
*
 GOUKEI-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ読込み                             *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE    "HMEIMS-READ-SEC"  TO     S-NAME.
*商品名称マスタ読込み
     READ    HMEIMS
             INVALID
             MOVE    "INV"      TO     HMEIMS-INV-FLG
             NOT  INVALID
             MOVE    SPACE      TO     HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
********************<<  PUROGRAM  END  >>*************************
