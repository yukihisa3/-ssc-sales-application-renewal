****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ユニディーオンラインシステム　　　*
*    モジュール名　　　　：　検収データリスト発行　　　　　　　*
*    作成日／更新日　　　：　97/09/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　検収（物品受領書）データの発行　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA1170B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          97/09/18.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YA        IS   PT-2
         YB        IS   PT-15
         YB-21     IS   PT-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    ユニディー検収データファイル
     SELECT   YNKENSYU     ASSIGN    TO   YNKENSYU
                           ACCESS    MODE IS   SEQUENTIAL
                           FILE STATUS    IS   KEN-ST.
*    店舗マスタ
     SELECT   TENMS1       ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TEN-F52 TEN-F011
                           STATUS    IS        TEN-ST.
*    商品コード変換テーブル
     SELECT      HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       STB-F01   STB-F02
                           STATUS    IS        STB-ST.
*    商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011  MEI-F0121
                                               MEI-F0122 MEI-F0123
                           STATUS    IS        MEI-ST.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO        LP-04.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
 FD  YNKENSYU            BLOCK CONTAINS   5  RECORDS.
 01  KEN-REC.
     03  KEN01                    PIC       X(01).
     03  KEN02                    PIC       X(02).
     03  KEN03                    PIC       X(06).
     03  KEN04                    PIC       X(09).
     03  KEN05                    PIC       X(02).
     03  KEN06                    PIC       X(05).
     03  KEN07                    PIC       X(02).
     03  KEN08                    PIC       X(01).
     03  KEN09.
         05  KEN091               PIC       X(02).
         05  KEN092               PIC       X(04).
     03  KEN10                    PIC       X(13).
     03  KEN11                    PIC       X(07).
     03  KEN12.
         05  KEN121               PIC       X(02).
         05  KEN122               PIC       X(02).
         05  KEN123               PIC       X(02).
     03  KEN13                    PIC       9(05).
     03  KEN14                    PIC       9(05).
     03  KEN15                    PIC       9(05).
     03  KEN16                    PIC       9(05).
     03  KEN17                    PIC       9(07).
     03  KEN18                    PIC       9(07).
     03  KEN19                    PIC       9(10).
     03  KEN20                    PIC       9(10).
     03  KEN21                    PIC       9(08).
     03  KEN22                    PIC       9(01)V99.
     03  KEN23                    PIC       X(01).
     03  FILLER                   PIC       X(74).
*
*****＜店舗マスタ＞*****
 FD  TENMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*****＜商品コード変換テーブル＞*****
 FD  HSHOTBL
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HSHOTBL   OF        XFDLIB
     JOINING     STB       AS        PREFIX.
*****＜商品名称マスタ＞*****
 FD  HMEIMS
     BLOCK       CONTAINS  1         RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*
*****＜画面　ファイル＞*****
 FD  DSPFILE.
*
     COPY    FVDA1170   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   33   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  STB-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  日付エリア
*01  WK-SYSDT.
*    03  WK-SYS-YY                PIC  9(02)     VALUE   ZERO.
*    03  WK-SYS-MM                PIC  9(02)     VALUE   ZERO.
*    03  WK-SYS-DD                PIC  9(02)     VALUE   ZERO.
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
***  変換エリア
 01  WK-TOKCD                     PIC  X(06).
 01  WK-TOKCDR                    REDEFINES    WK-TOKCD.
     03  WK-HEN-TOKCD             PIC  9(06).
 01  WK-TENPO                     PIC  X(02).
 01  WK-TENPOR                    REDEFINES    WK-TENPO.
     03  WK-HEN-TENPO             PIC  9(02).
 01  WK-DENNO                     PIC  X(08).
 01  WK-DENNOR                    REDEFINES    WK-DENNO.
     03  WK-HEN-DENNO             PIC  9(08).
 01  WK-ST-DENNO                  PIC  9(08).
 01  WK-DENPYO                    PIC  X(09).
 01  WK-ST-TOKCD                  PIC  9(06).
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  M-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  PAGE-CNT                 PIC  9(04)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  WK-SURYO                 PIC  9(05)     VALUE   ZERO.
     03  WK-ZENKAI                PIC  9(05)     VALUE   ZERO.
     03  WK-KONKAI                PIC  9(05)     VALUE   ZERO.
     03  WK-KEPIN                 PIC  9(05)     VALUE   ZERO.
     03  WK-GENKIN                PIC  9(10)     VALUE   ZERO.
     03  WK-BAIKIN                PIC  9(10)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)V9   VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*    03  W-SURYO                  PIC  9(06)V9   VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-KOTAE                 PIC  9(04)     VALUE   ZERO.
     03  WK-AMARI                 PIC  9(04)     VALUE   ZERO.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  AAA                      PIC  X(01)     VALUE   SPACE.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  DENNO1               PIC  9(10)     VALUE   ZERO.
         05  SINSEINO             PIC  9(08)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"検収データリスト出力中！！！".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(11)     VALUE
         NC"プリンター　　異常！！".
 01  FILE-ERR020                  PIC  N(11)     VALUE
         NC"店舗マスタ　　異常！！".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"商品変換ＴＢＬ異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"商品名称マスタ異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"検収データＦ　異常！！".
 01  FILE-ERR060                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  HEAD-01-ST          CHARACTER  TYPE  IS  PT-21.
         05  FILLER          PIC  X(40) VALUE   SPACE.
         05  FILLER          PIC  N(17) VALUE
             NC"＊＊　_ユニリビング　検収書　＊＊".
         05  FILLER          PIC  X(19) VALUE   SPACE.
         05  FILLER          PIC  X(05) VALUE   "DATE:".
         05  HD01-YY         PIC  99.
         05  FILLER          PIC  X(01) VALUE   "/".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  X(01) VALUE   "/".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  X(02) VALUE   SPACE.
         05  FILLER          PIC  X(05) VALUE   "PAGE:".
         05  HD01-PAGE       PIC  ZZZ9.
*
 01  HEAD-02.
     03  HEAD-02-ST          CHARACTER   TYPE  IS  PT-2.
         05  FILLER          PIC  X(03) VALUE   SPACE.
         05  FILLER          PIC  N(03) VALUE NC"社名：".
         05  HD02-SYACD      PIC  X(06).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  X(12) VALUE "ｶﾌﾞ)ﾕﾆﾘﾋﾞﾝｸﾞ".
         05  FILLER          PIC  X(04) VALUE   SPACE.
         05  FILLER          PIC  N(05) VALUE NC"伝票区分：".
         05  HD02-DENKU      PIC  X(02).
*
 01  HEAD-03.
     03  HEAD-03-ST          CHARACTER   TYPE  IS  PT-2.
         05  FILLER          PIC  X(03) VALUE   SPACE.
         05  FILLER          PIC  N(03) VALUE NC"店名：".
         05  HD03-TENCD      PIC  X(05).
         05  FILLER          PIC  X(02) VALUE   SPACE.
         05  HD03-TENNM      PIC  X(15).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  N(03) VALUE NC"部門：".
         05  HD03-BUMON      PIC  X(02).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  N(04) VALUE NC"中分類：".
         05  HD03-BUNRUI     PIC  X(04).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  N(05) VALUE NC"伝票番号：".
         05  HD03-DENNO      PIC  X(09).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  N(05) VALUE NC"検収日：".
         05  HD03-KEN-YY     PIC  99.
         05  FILLER          PIC  X(01) VALUE   "/".
         05  HD03-KEN-MM     PIC  Z9.
         05  FILLER          PIC  X(01) VALUE   "/".
         05  HD03-KEN-DD     PIC  Z9.
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  FILLER          PIC  N(05) VALUE NC"特売区分：".
         05  HD03-TOKUBAI    PIC  X(01).
         05  FILLER          PIC  X(01) VALUE   SPACE.
         05  HD03-TOKUNM     PIC  N(05).
*
 01  HEAD-04.
     03  FILLER              PIC  X(03) VALUE   SPACE.
     03  FILLER              PIC  N(05) VALUE NC"商　品　名"
                             CHARACTER   TYPE  IS  PT-2.
     03  FILLER              PIC  X(07) VALUE   SPACE.
     03  FILLER              PIC  N(06) VALUE NC"商品／ＪＡＮ"
                             CHARACTER   TYPE  IS  PT-2.
     03  FILLER              PIC  X(04) VALUE   "ｺｰﾄﾞ".
     03  FILLER              PIC  X(03) VALUE   SPACE.
     03  FILLER              PIC  N(02) VALUE NC"数量"
                             CHARACTER   TYPE  IS  PT-2.
     03  FILLER              PIC  X(02) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"前回検収"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(02) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"今回検収"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(02) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"欠品数量"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(05) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"原価単価"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(08) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"原価金額"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(04) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"売価単価"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(08) VALUE   SPACE.
     03  FILLER              PIC  N(04) VALUE NC"売価金額"
                             CHARACTER  TYPE  IS  PT-15.
     03  FILLER              PIC  X(04) VALUE   SPACE.
     03  FILLER              PIC  X(02) VALUE   "TC".
     03  FILLER              PIC  N(06) VALUE NC"利用料金／率"
                             CHARACTER  TYPE  IS  PT-15.
*    ﾒｲｻｲ
 01  BODY-01.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-SYONM1              PIC  X(15).
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  BD01-SYOCD               PIC  X(07).
*
 01  BODY-02.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD02-SYONM2              PIC  X(15).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-KAKO1               PIC  X(01).
     03  BD02-JANCD               PIC  X(13).
     03  BD02-KAKO2               PIC  X(01).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-SURYO               PIC  ZZ,ZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-ZENKAI              PIC  ZZ,ZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-KONKAI              PIC  ZZ,ZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-KEPIN               PIC  ZZ,ZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD02-GENTAN              PIC  Z,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-GENKIN              PIC  Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-BAITAN              PIC  Z,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-BAIKIN              PIC  Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-RIYO                PIC  ZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-RIYORITU            PIC  9.99.
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  TAIL-01-ST               CHARACTER  TYPE  IS  PT-2.
         05  FILLER               PIC  X(30)     VALUE   SPACE.
         05  FILLER               PIC  N(03) VALUE  NC"合　計".
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  TL01-SURYO-KEI       PIC  ZZ,ZZ9.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
         05  TL01-ZENKAI-KEI      PIC  ZZ,ZZ9.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
         05  TL01-KONAKI-KEI      PIC  ZZ,ZZ9.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
         05  TL01-KEPIN-KEI       PIC  ZZ,ZZ9.
         05  FILLER               PIC  X(12)     VALUE   SPACE.
         05  TL01-GENKIN-KEI      PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER               PIC  X(11)     VALUE   SPACE.
         05  TL01-BAIKIN-KEI      PIC  Z,ZZZ,ZZZ,ZZ9.
*    ﾃｲﾙ02
 01  TAIL-02.
     03  FILLER                   PIC  X(50)     VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC  X(50)     VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC  X(36)     VALUE
         "------------------------------------".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << プレインター >> ---*
 001-PRINTF-ERR         SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRINTF.
     DISPLAY  FILE-ERR010      UPON    CONS.
     STOP     RUN.
*--- << 店舗マスタ >> ---*
 002-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      TENMS1.
     DISPLAY  FILE-ERR020      UPON    CONS.
     DISPLAY  TEN-ST           UPON    CONS.
     STOP     RUN.
*--- << 商品変換テーブル >> ---*
 003-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HSHOTBL.
     DISPLAY  FILE-ERR030      UPON    CONS.
     DISPLAY  STB-ST           UPON    CONS.
     STOP     RUN.
*--- << 商品名称マスタ >> ---*
 004-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HMEIMS.
     DISPLAY  FILE-ERR040      UPON    CONS.
     DISPLAY  MEI-ST           UPON    CONS.
     STOP     RUN.
*--- << 検収データ >> ---*
 005-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      YNKENSYU.
     DISPLAY  FILE-ERR050      UPON    CONS.
     DISPLAY  KEN-ST           UPON    CONS.
     STOP     RUN.
*--- << 画面 >> ---*
 006-DSPFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR060      UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     DISPLAY "***** VDA1170B START *****" UPON CONS.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
     DISPLAY "***** VDA1170B END   *****" UPON CONS.
     STOP     RUN.
*
 CONTROL-END.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     YNKENSYU  TENMS1  HSHOTBL  HMEIMS.
     OPEN     I-O       DSPFILE.
*    日付取得
*    ACCEPT   WK-SYSDT  FROM  DATE.
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
*    ワーク初期化
     MOVE     ZERO               TO   DENNO1.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FVDA1170.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FVDA1170.
*    画面表示
     PERFORM       DSP-WRITE-SEC.
*    キー入力
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*処理対象未入力時処理
     IF   MD04  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN-SEC
     ELSE
          IF    MD04  <   2
          AND   MD04  >   3
                MOVE     2    TO   ERR-FLG
                PERFORM       ERR-MSG-SEC
                GO       TO   MAIN-SEC
          END-IF
          MOVE  MD04    TO   WRK-R040
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        テストプリント
*********WHEN      1
*******************PERFORM   TEST-PRT-SEC
*        全プリント
         WHEN      2
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
*        範囲指定
         WHEN      3
                   PERFORM   KEY-IN-SEC
                   IF        END-FLG   =   "END"
                             GO        TO   MAIN-EXIT
                   END-IF
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
***                          UNTIL     END-FLG   =   "END"
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
*    奇数頁出力時ストックホーム制御処理
     IF       PAGE-CNT  NOT =  ZERO
              DIVIDE   PAGE-CNT  BY  2  GIVING    WK-KOTAE
                                        REMAINDER WK-AMARI
              IF       WK-AMARI  =  ZERO
                       MOVE   SPACE   TO     PRT-REC
                       WRITE  PRT-REC AFTER  PAGE
              END-IF
     END-IF.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    YNKENSYU.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO       RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     YNKENSYU.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             エラーメッセージセット                2.1        *
****************************************************************
 ERR-MSG-SEC            SECTION.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   MD05
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   MD05
              MOVE   ZERO                  TO   ERR-FLG
     END-IF.
*
 ERR-MSG-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRITE-SEC          SECTION.
*    伝票総枚数セット
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FVDA1170" TO        DSP-FMT.
     MOVE     HEN-DATE  TO        SDATE.
     MOVE     HEN-TIME  TO        STIME.
*    画面表示
     WRITE    FVDA1170.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-READ-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
*
 KEY-IN-01.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    範囲指定
     MOVE         "KEY01"    TO   DSP-GRP.
     MOVE          ZERO      TO   MD02      MD03.
*    画面読込み
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
*        終了
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
*        実行
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
*    範囲指定大小チェック
     IF            MD02   >  MD03
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-IF.
*    未入力時（全データ対象）
     IF           (MD02   =  ZERO)     AND
                  (MD03   =  ZERO)
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
     END-IF.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                          2.5        *
****************************************************************
 DENP-WT-SEC            SECTION.
*伝票_のブレイク判定
     IF       KEN04     NOT =     WK-DENPYO
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      KEN04    TO   WK-DENPYO
     END-IF.
*項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*明細項目セット
*商品名称取得
     MOVE     KEN03              TO   WK-TOKCD.
     MOVE     WK-HEN-TOKCD       TO   STB-F01.
     MOVE     KEN10              TO   STB-F02.
     PERFORM  SHOTBL-RD-SEC.
*商品コード
     MOVE     KEN11              TO   BD01-SYOCD.
*ＪＡＮコード
     MOVE     "("                TO   BD02-KAKO1.
     MOVE     KEN10              TO   BD02-JANCD.
     MOVE     ")"                TO   BD02-KAKO2.
*数量
     MOVE     KEN13              TO   BD02-SURYO.
     ADD      KEN13              TO   WK-SURYO.
*前回検収数量
     MOVE     KEN14              TO   BD02-ZENKAI.
     ADD      KEN14              TO   WK-ZENKAI.
*今回検収数量
     MOVE     KEN15              TO   BD02-KONKAI.
     ADD      KEN15              TO   WK-KONKAI.
*欠品数量
     MOVE     KEN16              TO   BD02-KEPIN.
     ADD      KEN16              TO   WK-KEPIN.
*原価単価
     MOVE     KEN17              TO   BD02-GENTAN.
*原価金額
     MOVE     KEN19              TO   BD02-GENKIN.
     ADD      KEN19              TO   WK-GENKIN.
*売価単価
     MOVE     KEN18              TO   BD02-BAITAN.
*売価金額
     MOVE     KEN20              TO   BD02-BAIKIN.
     ADD      KEN20              TO   WK-BAIKIN.
*ＴＣ利用料金
     MOVE     KEN21              TO   BD02-RIYO.
*ＴＣ利用率表
     MOVE     KEN22              TO   BD02-RIYORITU.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT.
     ADD      2                  TO   M-CNT.
*
     PERFORM  FL-READ-SEC.
*
     IF       SYORI-FLG   =        "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     YNKENSYU  AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-READ.
*    全プリント時判定
     IF       MD04    =    2
              GO                  TO   READ-010
     END-IF.
*    伝票番号範囲指定大小チェック
     MOVE     KEN04               TO   WK-DENNO.
     MOVE     WK-HEN-DENNO        TO   WK-ST-DENNO.
**** DISPLAY "WK-ST-DENNO = " WK-ST-DENNO  UPON CONS.
**** DISPLAY "MD02        = " MD02         UPON CONS.
     IF       WK-ST-DENNO  <    MD02
              GO                  TO   READ-000
     END-IF.
     IF       WK-ST-DENNO  >    MD03
              GO                  TO   READ-000
     END-IF.
 READ-010.
*    １件目時処理
     IF       RD-SW     =    0
              MOVE      KEN04     TO   WK-DENPYO
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             商品名称取得処理                      2.5.1      *
****************************************************************
 SHOTBL-RD-SEC          SECTION.
*    商品変換テーブル読込み
     READ     HSHOTBL
              INVALID     MOVE   ALL "*"  TO  BD01-SYONM1
                          MOVE   ALL "*"  TO  BD02-SYONM2
                          GO              TO  SHOTBL-RD-EXIT
     END-READ.
*商品名称マスタ読込み
     MOVE     STB-F031            TO   MEI-F011.
     MOVE     STB-F032(1:5)       TO   MEI-F0121.
     MOVE     STB-F032(6:2)       TO   MEI-F0122.
     MOVE     STB-F032(8:1)       TO   MEI-F0123.
     READ     HMEIMS
              INVALID     MOVE   ALL "*"  TO  BD01-SYONM1
                          MOVE   ALL "*"  TO  BD02-SYONM2
              NOT INVALID MOVE   MEI-F031 TO  BD01-SYONM1
                          MOVE   MEI-F032 TO  BD02-SYONM2
     END-READ.
*
 SHOTBL-RD-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*    ヘッド部項目初期化
     INITIALIZE                  HEAD-01
                                 HEAD-02
                                 HEAD-03
                                 HEAD-04.
*日付
     MOVE      WK-Y              TO    HD01-YY.
     MOVE      WK-M              TO    HD01-MM.
     MOVE      WK-D              TO    HD01-DD.
*頁
     ADD       1                 TO    PAGE-CNT.
     MOVE      PAGE-CNT          TO    HD01-PAGE.
*社名
     MOVE      KEN03             TO    HD02-SYACD.
*伝票区分
     MOVE      KEN07             TO    HD02-DENKU.
*店舗
     MOVE      KEN06             TO    HD03-TENCD.
     MOVE      KEN03             TO    WK-TOKCD.
     MOVE      WK-HEN-TOKCD      TO    WK-ST-TOKCD.
     MOVE      WK-HEN-TOKCD      TO    TEN-F52.
     MOVE      KEN06             TO    WK-TENPO.
     MOVE      WK-HEN-TENPO      TO    TEN-F011.
***  DISPLAY "TOKCD = " WK-HEN-TOKCD UPON CONS.
***  DISPLAY "TENPO = " WK-HEN-TENPO UPON CONS.
*店舗マスタ読込み
     READ      TENMS1
               INVALID      MOVE  ALL "*"   TO   HD03-TENNM
               NOT INVALID  MOVE  TEN-F04   TO   HD03-TENNM
     END-READ.
*部門
     MOVE     KEN091             TO    HD03-BUMON.
*中分類
     MOVE     KEN092             TO    HD03-BUNRUI.
*伝票番号
     MOVE     KEN04              TO    HD03-DENNO
*中分類
     MOVE     KEN092             TO    HD03-BUNRUI.
*検収日
     MOVE     KEN12(1:2)         TO    HD03-KEN-YY.
     MOVE     KEN12(3:2)         TO    HD03-KEN-MM.
     MOVE     KEN12(5:2)         TO    HD03-KEN-DD.
*特売区分
     MOVE     KEN08              TO    HD03-TOKUBAI.
     EVALUATE KEN08
         WHEN "1"
         MOVE NC"特売商品"       TO    HD03-TOKUNM
         WHEN "2"
         MOVE NC"スポット品"     TO    HD03-TOKUNM
         WHEN " "
         MOVE NC"定番商品"       TO    HD03-TOKUNM
         WHEN OTHER
         MOVE NC"＊＊＊＊"       TO    HD03-TOKUNM
     END-EVALUATE.
*
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    2.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    2.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-04    AFTER    2.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  1.
*    行カウンタ
     MOVE      7               TO     L-CNT.
     MOVE      ZERO            TO     M-CNT.
*    合計エリアクリア
     MOVE      ZERO            TO  WK-SURYO WK-ZENKAI WK-KONKAI.
     MOVE      ZERO            TO  WK-KEPIN WK-GENKIN WK-BAIKIN.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TAIL-01.
*    原価金額合計
     MOVE   WK-SURYO             TO   TL01-SURYO-KEI.
     MOVE   WK-ZENKAI            TO   TL01-ZENKAI-KEI.
     MOVE   WK-KONKAI            TO   TL01-KONAKI-KEI.
     MOVE   WK-KEPIN             TO   TL01-KEPIN-KEI.
     MOVE   WK-GENKIN            TO   TL01-GENKIN-KEI.
     MOVE   WK-BAIKIN            TO   TL01-BAIKIN-KEI.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     18  -  M-CNT  +  2.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
     WRITE    PRT-REC          FROM   TAIL-02   AFTER   2.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*    ワーク初期化
     MOVE     ZERO               TO   WK-SURYO
                                      WK-ZENKAI
                                      WK-KONKAI
                                      WK-KEPIN
                                      WK-BAIKIN
                                      WK-GENKIN.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*    ファイルのＣＬＯＳＥ
     CLOSE    YNKENSYU TENMS1 HSHOTBL HMEIMS DSPFILE.
*
 END-EXIT.
     EXIT.
******************************************************************
*END PROGRAM  VDA1170B.
******************************************************************
