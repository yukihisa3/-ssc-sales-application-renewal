# SIT0060L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0060L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理システム                  *
*    業務名　　　　　　　：　仕入先マスタ　　　　　　　        *
*    モジュール名　　　　：　仕入先マスタリスト                *
*    作成日／作成者　　　：　93/04/14  /NAV                    *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*          ・新郵便番号追加                                    *
*          ・仕入先マスタをＦＤＧより参照するよう修正          *
*          ・日付８桁変換ＰＧ（ＳＫＹＤＴＣＫＢ）使用          *
*    作成日／作成者　　　：　09/03/23  /NAV IMAI               *
*          ・ＩＴ統制対応　　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ZMT0040B→SIT0060L　　　　　　
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0060L.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        MSTLOGL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-ST.
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
*-----------------------------------------*
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*担当者マスタ
 FD  HTANMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
****<<  プリントファイル  >>********************************
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(2).
     02 DSP-GROUP            PIC  X(8).
     02 DSP-FORMAT           PIC  X(8).
     02 DSP-STATUS           PIC  X(2).
     02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 MSL-ST               PIC  X(2).
     02 TAN-ST               PIC  X(02).
****  カウンタ                ****
 01  L-CNT                   PIC  9(2).
 01  P-CNT                   PIC  9(3)   VALUE  1.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
*----------- 99/09/28修正 ---------------------*
****  日付保存                ****
*01  SYS-DATE                PIC  9(06).
*01  SYS-DATE8.
*    03  SYS-YYYY            PIC  9(04).
*    03  SYS-MM              PIC  9(02).
*    03  SYS-DD              PIC  9(02).
*----------------------------------------------*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*
 01  WK-SEIYY.
     03  WK-YY1                   PIC  9(02).
     03  WK-YY2                   PIC  9(02).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(34)  VALUE  SPACE.
     02  FILLER              PIC  N(17)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　仕入先マスタリスト　＊＊＊".
     02  FILLER              PIC  X(15)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"処理日".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  H-YY                PIC  9(04).
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-MM                PIC  Z9.
     02  FILLER              PIC  X(1)   VALUE  ".".
     02  H-DD                PIC  Z9.
     02  FILLER              PIC  X(3)   VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE  NC"頁".
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  PAGE-SUU            PIC  ZZ9.
*見出し１１
 01  MIDASI-11.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
                                   "+------+------+------+".
*見出し１２
 01  MIDASI-12               CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　".
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"責任者".
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"担当者".
     02  FILLER              PIC  X(01)  VALUE  "!".
*見出し１３
 01  MIDASI-13.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
                                   "+------+------+------+".
*見出し１４
 01  MIDASI-14.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
*見出し１５
 01  MIDASI-15.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)  VALUE  NC"　　　"
         CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  "!".
*見出し１６
 01  MIDASI-16.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
                                   "+------+------+------+".
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(12)  VALUE
         NC"仕入先　仕入先名（漢字）".
     02  FILLER              PIC  X(12)  VALUE  SPACE.
     02  FILLER              PIC  N(08)  VALUE
         NC"担当部課（漢字）".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"〒".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　１".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"住　所　２".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＴＥＬ".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
****  見出し行３             ****
 01  MIDASI-3       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(17)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"（カナ）".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"県コード".
     02  FILLER              PIC  X(50)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"ＦＡＸ".
****  見出し行４             ****
 01  MIDASI-4       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  SHI-01              PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-02              PIC  N(18).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-04              PIC  N(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-121             PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  SHI-122             PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-07              PIC  N(18).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SHI-08              PIC  N(18).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  SHI-09              PIC  X(15).
****  明細行２               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  SHI-03              PIC  X(25).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  SHI-05              PIC  X(15).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHI-06              PIC  X(05).
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  SHI-11              PIC  9(02).
     02  FILLER              PIC  X(56)  VALUE  SPACE.
     02  SHI-10              PIC  X(15).
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  L-SYORI             PIC  N(04).
     02  L-TANCD             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  L-TANNM             PIC  N(11).
     02  L-UPDT              PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  L-UPTM              PIC  X(08).
     02  FILLER              PIC  N(01)  VALUE  NC"］".
**** エラーメッセージ         ****
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"開始・終了コードの関係に誤りがあります。".
     02  PMSG01              PIC N(20) VALUE
            NC"■取消　■終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0060L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER            PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM       PIC  N(02).
     03  FILLER            PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM         PIC  N(10).
     03  FILLER            PIC  N(01)  VALUE NC"：".
*
 01  READ-CNT              PIC  9(07)  VALUE 0.
 01  IN-CNT                PIC  9(07)  VALUE 0.
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*マスタレコードエリア（仕入マスタ）
     COPY    ZSHIMS       OF   XFDLIB
     JOINING SHI           AS   PREFIX.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
 01  PARA-UPDTDATE-E       PIC 9(08).
 01  PARA-UPDTIME-E        PIC 9(06).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION USING PARA-BUMONCD
                                       PARA-TANCD
                                       PARA-UPDTDATE
                                       PARA-UPDTIME
                                       PARA-UPDTDATE-E
                                       PARA-UPDTIME-E.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MSTLOGF.
     MOVE   "MSTLOGF "        TO    ERR-FL-ID.
     MOVE    MSL-ST           TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HTANMS.
     MOVE   "HTANMS "         TO    ERR-FL-ID.
     MOVE    TAN-ST           TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 ZMT0040B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZMT0040B-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*パラメタ：担当者コード取得
     MOVE     PARA-TANCD          TO   WK-TANCD.
*
     OPEN     INPUT     MSTLOGF HTANMS.
     OPEN     OUTPUT    PRINTF.
*
*----------------- 99/09/28追加 ---------------------*
*システム日付取得
*    ACCEPT   SYS-DATE          FROM   DATE.
*    MOVE     "3"                 TO   LINK-IN-KBN.
*    MOVE     SYS-DATE            TO   LINK-IN-YMD6.
*    MOVE     ZERO                TO   LINK-IN-YMD8.
*    MOVE     ZERO                TO   LINK-OUT-RET.
*    MOVE     ZERO                TO   LINK-OUT-YMD.
*    CALL     "SKYDTCKB"       USING   LINK-IN-KBN
*                                      LINK-IN-YMD6
*                                      LINK-IN-YMD8
*                                      LINK-OUT-RET
*                                      LINK-OUT-YMD.
*    MOVE      LINK-OUT-YMD       TO   SYS-DATE8.
*----------------------------------------------------*
*
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "02"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02
     MOVE     WK-TANCD            TO   MSL-F03.
     MOVE     PARA-UPDTDATE       TO   MSL-F05.
     MOVE     PARA-UPDTIME        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F05
                                           MSL-F06
              INVALID  MOVE  "END"    TO   END-FLG
                       GO TO      INIT-END.
     PERFORM  READMSL-SEC.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     IF       L-CNT         >=      62
              PERFORM       MIDASI-SEC
     END-IF.
     ADD      1               TO      IN-CNT.
     MOVE     MSL-F08         TO      SHI-REC.
     MOVE     SHI-F01         TO      SHI-01.
     MOVE     SHI-F02         TO      SHI-02.
     MOVE     SHI-F03         TO      SHI-03.
     MOVE     SHI-F04         TO      SHI-04.
     MOVE     SHI-F05         TO      SHI-05.
     MOVE     SHI-F121        TO      SHI-121.
     MOVE     SHI-F122        TO      SHI-122.
     MOVE     SHI-F06         TO      SHI-06.
     MOVE     SHI-F07         TO      SHI-07.
     MOVE     SHI-F08         TO      SHI-08.
     MOVE     SHI-F09         TO      SHI-09.
     MOVE     SHI-F10         TO      SHI-10.
     MOVE     SHI-F11         TO      SHI-11.
*
*更新履歴
     EVALUATE    MSL-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   L-SYORI.
*
     MOVE        MSL-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   L-TANCD.
*担当者名取得
     MOVE        MSL-F02                TO   TAN-F01.
     MOVE        WK-TANCD1              TO   TAN-F02.
     READ        HTANMS
       INVALID
                 MOVE   SPACE           TO  HEN-TANNM
       NOT INVALID
                 MOVE   TAN-F03         TO  HEN-TANNM
     END-READ.
*
     MOVE        HEN-TAN                TO  L-TANNM.
     MOVE        MSL-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        MSL-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        MSL-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  L-UPDT.
*
     MOVE        MSL-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        MSL-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        MSL-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  L-UPTM.
*
     WRITE    P-REC         FROM    MEISAI-1    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-2    AFTER  1.
     WRITE    P-REC         FROM    MEISAI-3    AFTER  1.
     WRITE    P-REC         FROM    SEN1        AFTER  1.
     ADD      4             TO      L-CNT.
*
     PERFORM  READMSL-SEC.
 MAIN-END.
     EXIT.
**********************
*マスタ更新履歴ファイルＲＥＡＤ*
**********************
 READMSL-SEC               SECTION.
*
     READ   MSTLOGF        AT        END
         MOVE    "END"     TO        END-FLG
         GO                TO        READMSL-EXIT.
*
     MOVE   MSL-F05        TO        TRND-DATE.
     MOVE   MSL-F06        TO        TRND-TIME.
     MOVE   PARA-UPDTDATE-E TO       TO-DATE.
     MOVE   PARA-UPDTIME-E  TO       TO-TIME.
     IF  (MSL-F01  >  "02") OR (MSL-F02  >  PARA-BUMONCD) OR
         (MSL-F03  >  PARA-TANCD) OR (TRND-DT > TO-DT)
         MOVE    "END"     TO        END-FLG
         GO TO   READMSL-EXIT.
     ADD    1              TO     READ-CNT.
*
 READMSL-EXIT.
     EXIT.
************************************************************
*      2.1       見出し処理                                *
************************************************************
 MIDASI-SEC             SECTION.
     MOVE     WK-YS      TO      WK-YY1.
     MOVE     WK-Y       TO      WK-YY2.
     MOVE     WK-SEIYY   TO      H-YY.
     MOVE     WK-M       TO      H-MM.
     MOVE     WK-D       TO      H-DD.
     MOVE     P-CNT      TO      PAGE-SUU.
     IF       P-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-11  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-14  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-16  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-11  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-14  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-16  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-3   AFTER  1
              WRITE      P-REC   FROM    MIDASI-4   AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
     END-IF.
     ADD      1  TO      P-CNT.
     MOVE     12 TO      L-CNT.
 MIDASI-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    PRINTF   MSTLOGF  HTANMS.
     DISPLAY  "SIT0060L READ =" READ-CNT UPON CONS.
     DISPLAY  "SIT0060L IN   =" IN-CNT   UPON CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
