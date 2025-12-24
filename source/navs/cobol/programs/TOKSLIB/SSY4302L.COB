****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　物品受領書発行                    *
*    作成日／更新日　　　：　02/01/13                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            物品受領書を発行する。            *
*                            アークランドサカモト　　          *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY4302L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/01/13.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   PICH-YA
         YB        IS   PICH-YB
         YB-21     IS   PICH-YA21.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    検収データＦ
     SELECT   CVCSG001           ASSIGN    TO   CVCSG001
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   DEN-STATUS.
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
*    FILE = ｹﾝｼｭｳﾃﾞｰﾀﾌｱｲﾙ
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEN-REC.
     03  DEN-01                   PIC  X(03).
     03  DEN-02                   PIC  X(125).
*
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FSY43021   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   66   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
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
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  MEISAI-CNT               PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
***  合計集計ｴﾘｱ
 01  WRK-GOKEI.
     03  GK-HATYU                 PIC  9(07)V9   VALUE   ZERO.
     03  GK-KENSYU                PIC  9(07)V9   VALUE   ZERO.
     03  GK-GENKAKEI              PIC  9(09)     VALUE   ZERO.
     03  GK-KENSYUKEI             PIC  9(09)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"物品受領書データが存在しません。".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(14)     VALUE
         NC"印刷を続けますか？　Ｙ／Ｎ".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY4302L".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  X(08)     VALUE "SSY4302L".
     03  FILLER                   PIC  X(25)     VALUE   SPACE.
     03  FILLER                   PIC  N(19)     VALUE
         NC"＊＊アークランドサカモト物品受領書＊＊"
         CHARACTER  TYPE  IS  PICH-YA21.
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  FILLER                   PIC  X(05)     VALUE   "DATE:".
     03  SYS-DT-YY                PIC  9(04).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  SYS-DT-MM                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  SYS-DT-DD                PIC  9(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  X(05)     VALUE   "PAGE:".
     03  PG-CNT                   PIC  ZZZ9.
*
 01  HEAD-02                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(13)     VALUE
         NC"社名：アークランドサカモト".
*
 01  HEAD-03                      CHARACTER  TYPE  IS  PICH-YB.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票番号：".
     03  HD03-DENNO               PIC  99999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票区分：".
     03  HD03-DENKU               PIC  99.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD03-DENKUNM             PIC  N(04).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"部門：".
     03  HD03-BUMON               PIC  9999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"納品区分：".
     03  HD03-NOUKU               PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD03-NOUKN               PIC  N(10).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"検品区分：".
     03  HD03-KENPIN              PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD03-KENPINNM            PIC  N(03).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"納品店舗：".
     03  HD03-NTENCD              PIC  9999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD03-NTENNM              PIC  X(10).
     03  FILLER                   PIC  X(01)     VALUE   ")".
*
 01  HEAD-04                      CHARACTER  TYPE  IS  PICH-YB.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"発注日：".
     03  HD04-HDATE               PIC  99999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"納品日：".
     03  HD04-NDATE               PIC  99999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"納品時刻：".
     03  HD04-NTIME               PIC  9999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"便：".
     03  HD04-BIN                 PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"値付：".
     03  HD04-NETUKE              PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD04-NETUKENM            PIC  N(04).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"特売：".
     03  HD04-TOKUBAI             PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD04-TOKUBAINM           PIC  N(02).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"発注：".
     03  HD04-HATYU               PIC  9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD04-HATYUNM             PIC  N(02).
     03  FILLER                   PIC  X(01)     VALUE   ")".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"部門区分：".
     03  HD04-BUMONKBN            PIC  99999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"梱包店舗：".
     03  HD04-KTENCD              PIC  9999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   "(".
     03  HD04-KTENNM              PIC  X(10).
     03  FILLER                   PIC  X(01)     VALUE   ")".
*
 01  HEAD-05                      CHARACTER  TYPE  IS  PICH-YB.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票仕分：".
     03  HD05-SIWAKE              PIC  X.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"摘要：".
     03  HD05-TEKIYO              PIC  X(15).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"計上日：".
     03  HD05-KEIJYO              PIC  99999999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(06)     VALUE
         NC"検収担当者：".
     03  HD05-TANTO               PIC  X(10).
*
 01  HEAD-06                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(03)     VALUE   "SEQ".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(01)     VALUE
         NC"行".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"商品ＣＤ".
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"商品名称".
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"発注数量".
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  FILLER                   PIC  N(06)     VALUE
         NC"［検収数量］".
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価単価".
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価金額".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(06)     VALUE
         NC"［検収金額］".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"理由".
*
 01  BODY-01.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-SEQ                 PIC  ZZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-GYO                 PIC  Z9.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-SYOCD               PIC  X(10).
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  BD01-SYONM1              PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-HATYUSU             PIC  Z,ZZZ,ZZ9.9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-KENSYU-0            PIC  X(01).
     03  BD01-KENSYU              PIC  Z,ZZZ,ZZ9.9.
     03  BD01-KENSYU-1            PIC  X(01).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-GENKA               PIC  ZZ,ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-GENKIN              PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-KENKIN-0            PIC  X(01).
     03  BD01-KENKIN              PIC  ZZZ,ZZZ,ZZ9.
     03  BD01-KENKIN-1            PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-RIYU                PIC  X(10).
*
 01  BODY-02.
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  BD02-JANCD               PIC  X(13).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-SYONM2              PIC  X(20).
*    ｺﾞｳｹｲ
 01  TAIL-01                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(43)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE NC"合計".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  TL01-GHATYU              PIC  Z,ZZZ,ZZ9.9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  TL01-GKENSYU-0           PIC  X(01).
     03  TL01-GKENSYU             PIC  Z,ZZZ,ZZ9.9.
     03  TL01-GKENSYU-1           PIC  X(01).
     03  FILLER                   PIC  X(17)     VALUE   SPACE.
     03  TL01-GGENKIN             PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  TL01-GKENKIN-0           PIC  X(01).
     03  TL01-GKENKIN             PIC  ZZZ,ZZZ,ZZ9.
     03  TL01-GKENKIN-1           PIC  X(01).
*
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE ALL "=".
*
 01  SEN2.
     03  FILLER                  PIC  X(136) VALUE ALL "-".
*
*ヘッドレコード退避ワーク
 01  WK-HEAD-REC.
     03  WK-HEAD01          PIC  X(02).
     03  WK-HEAD02          PIC  X(01).
     03  WK-HEAD03          PIC  9(08).
     03  WK-HEAD04          PIC  9(08).
     03  WK-HEAD05          PIC  9(02).
     03  WK-HEAD06          PIC  9(04).
     03  WK-HEAD07          PIC  9(01).
     03  WK-HEAD08          PIC  9(01).
     03  WK-HEAD09          PIC  9(04).
     03  WK-HEAD10          PIC  9(04).
     03  WK-HEAD11          PIC  9(08).
     03  WK-HEAD12          PIC  9(08).
     03  WK-HEAD13          PIC  9(04).
     03  WK-HEAD14          PIC  9(01).
     03  WK-HEAD15          PIC  9(01).
     03  WK-HEAD16          PIC  9(01).
     03  WK-HEAD17          PIC  9(01).
     03  WK-HEAD18          PIC  9(05).
     03  WK-HEAD19          PIC  X(01).
     03  WK-HEAD20          PIC  X(10).
     03  WK-HEAD21          PIC  X(10).
     03  WK-HEAD22          PIC  X(15).
     03  WK-HEAD23          PIC  X(10).
     03  WK-HEAD24          PIC  9(08).
     03  WK-HEAD25          PIC  X(10).
*
*    明細レコード退避ワーク
 01  WK-BODY-REC.
     03  WK-BODY01          PIC  X(02).
     03  WK-BODY02          PIC  X(01).
     03  WK-BODY03          PIC  9(04).
     03  WK-BODY04          PIC  9(02).
     03  WK-BODY05          PIC  9(10).
     03  WK-BODY06          PIC  9(13).
     03  WK-BODY07          PIC  X(03).
     03  WK-BODY08          PIC  9(06)V9(01).
     03  WK-BODY09          PIC  9(06)V9(02).
     03  WK-BODY10          PIC  9(09).
     03  WK-BODY11          PIC  X(20).
     03  WK-BODY12          PIC  X(20).
     03  WK-BODY13          PIC  9(06)V9(01).
     03  WK-BODY14          PIC  9(09).
     03  WK-BODY15          PIC  X(10).
     03  WK-BODY16          PIC  X(03).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << ﾌﾟﾘﾝﾀ- ｴﾗ- >> ---*
 000-PRINTF-ERR         SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    PRINTF.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << ｼｲﾚ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    CVCSG001.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     MOVE      DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << ｶﾞﾒﾝ    ｴﾗ- >> ---*
 000-DSPFILE-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DSPFILE.
     MOVE     "DSPFFILE"       TO   ERR-FL-ID.
     MOVE      DSP-ST          TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR040   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FSY43021.
*    システム日付取得
     ACCEPT   WK-YMD    FROM     DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   WK-DATE.
*
*    伝票枚数カウント
 INIT001.
     READ     CVCSG001  AT   END
              GO        TO       INIT002
     END-READ.
*    伝票番号ブレイクチェック
     IF       DEN-01    =   "20B"
              ADD       1    TO    WRK-MAI
              GO             TO    INIT001
     ELSE
              GO             TO    INIT001
     END-IF.
*    伝票番号ゼロ件処理（エラーメッセージ出力）
 INIT002.
     IF    WRK-MAI  <=  0
           MOVE     MSG01     TO     MD05
     END-IF.
*    ファイルＯＰＥＮ／ＣＬＯＳＥ
     CLOSE          CVCSG001.
     OPEN   INPUT   CVCSG001.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FSY43021.
*    画面表示
     PERFORM       ERR-MSG-SEC.
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
          GO       TO   MAIN-SEC
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        全プリント
         WHEN      1
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
     IF       PAGE-SW   =    1
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    CVCSG001.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO   RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     CVCSG001.
*
     IF       ERR-FLG   =    ZERO
              MOVE     "END"      TO   END-FLG
     END-IF.
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
     MOVE     WRK-MAI   TO        MD01.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FSY43021" TO        DSP-FMT.
*    画面表示
     WRITE    FSY43021.
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
*             伝票出力処理                          2.5        *
****************************************************************
 DENP-WT-SEC            SECTION.
*    ヘッダ印字
     IF       DEN-01    =   "20B"
              IF   CNT-PAGE  >  ZERO
                   PERFORM  TAIL-WT-SEC
              END-IF
              MOVE      ZERO           TO   WRK-GOKEI
              MOVE      DEN-REC        TO   WK-HEAD-REC
              PERFORM   HEAD-WT-SEC
              GO        TO   DENP-WT-010
     END-IF.
     IF       DEN-01    =   "20D"
              MOVE      DEN-REC        TO   WK-BODY-REC
     ELSE
              GO        TO   DENP-WT-010
     END-IF.
*    項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*    明細項目セット
     ADD      1                  TO   CNT-GYO.
*    ＳＥＱ
     MOVE     WK-BODY03          TO   BD01-SEQ.
*    行番号
     MOVE     WK-BODY04          TO   BD01-GYO.
*    商品ＣＤ
     MOVE     WK-BODY05          TO   BD01-SYOCD.
*    ＪＡＮコード
     MOVE     WK-BODY06          TO   BD02-JANCD.
*    商品名称
     MOVE     WK-BODY11          TO   BD01-SYONM1.
     MOVE     WK-BODY12          TO   BD02-SYONM2.
*    発注数量
     MOVE     WK-BODY08          TO   BD01-HATYUSU.
     COMPUTE  GK-HATYU  =  GK-HATYU  +  WK-BODY08.
*    検収数量
     MOVE     "("                TO   BD01-KENSYU-0.
     MOVE     WK-BODY13          TO   BD01-KENSYU.
     MOVE     ")"                TO   BD01-KENSYU-1.
     COMPUTE  GK-KENSYU  =  GK-KENSYU  +  WK-BODY13.
*    原価単価
     MOVE     WK-BODY09          TO   BD01-GENKA.
*    原価金額
     MOVE     WK-BODY10          TO   BD01-GENKIN.
     COMPUTE  GK-GENKAKEI  =  GK-GENKAKEI  +  WK-BODY10.
*    検収金額
     MOVE     "("                TO   BD01-KENKIN-0
     MOVE     WK-BODY14          TO   BD01-KENKIN.
     MOVE     ")"                TO   BD01-KENKIN-1.
     COMPUTE  GK-KENSYUKEI  =  GK-KENSYUKEI  +  WK-BODY14.
*    理由
     MOVE     WK-BODY15          TO   BD01-RIYU.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT MEISAI-CNT.
*
 DENP-WT-010.
*
     PERFORM  FL-READ-SEC.
*ファイル終了時判定
     IF       SYORI-FLG  =  "END"
              PERFORM  TAIL-WT-SEC
     END-IF.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     CVCSG001  AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
     END-READ.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*    ヘッド部項目初期化
     INITIALIZE                  HEAD-01
                                 HEAD-02
                                 HEAD-03
                                 HEAD-04
                                 HEAD-05
                                 HEAD-06.
*--- 項目セット
*    日付セット
     MOVE     WK-DATE(1:4)     TO      SYS-DT-YY.
     MOVE     WK-DATE(5:2)     TO      SYS-DT-MM.
     MOVE     WK-DATE(7:2)     TO      SYS-DT-DD.
*    伝票番号
     MOVE     WK-HEAD04        TO      HD03-DENNO.
*    伝票区分
     MOVE     WK-HEAD05        TO      HD03-DENKU.
     EVALUATE WK-HEAD05
         WHEN 50    MOVE NC"仕入　　" TO   HD03-DENKUNM
         WHEN 50    MOVE NC"返品　　" TO   HD03-DENKUNM
         WHEN 50    MOVE NC"値引　　" TO   HD03-DENKUNM
         WHEN 50    MOVE NC"仕入訂正" TO   HD03-DENKUNM
         WHEN 50    MOVE NC"返品訂正" TO   HD03-DENKUNM
         WHEN 50    MOVE NC"値引訂正" TO   HD03-DENKUNM
         WHEN OTHER MOVE NC"＊＊＊＊" TO   HD03-DENKUNM
     END-EVALUATE.
*    部門
     MOVE     WK-HEAD06        TO      HD03-BUMON.
*    納品区分
     MOVE     WK-HEAD07        TO      HD03-NOUKU.
     EVALUATE WK-HEAD07
         WHEN 1
         MOVE NC"店納品　　　　　　　" TO HD03-NOUKN
         WHEN 2
         MOVE NC"店別梱包センター納品" TO HD03-NOUKN
         WHEN 3
         MOVE NC"一括梱包センター納品" TO HD03-NOUKN
         WHEN OTHER
         MOVE NC"＊＊＊＊＊＊＊＊＊＊" TO HD03-NOUKN
     END-EVALUATE.
*    検品区分
     MOVE     WK-HEAD08        TO      HD03-KENPIN.
     EVALUATE WK-HEAD08
         WHEN 1     MOVE NC"ピース" TO   HD03-KENPINNM
         WHEN 2     MOVE NC"ケース" TO   HD03-KENPINNM
         WHEN 3     MOVE NC"不定形" TO   HD03-KENPINNM
         WHEN OTHER MOVE NC"＊＊＊" TO   HD03-KENPINNM
     END-EVALUATE.
*    納品店舗
     MOVE     WK-HEAD09        TO      HD03-NTENCD.
     MOVE     WK-HEAD20        TO      HD03-NTENNM.
*    発注日
     MOVE     WK-HEAD11        TO      HD04-HDATE.
*    納品日
     MOVE     WK-HEAD12        TO      HD04-NDATE.
*    納品指定時刻
     MOVE     WK-HEAD13        TO      HD04-NTIME.
*    便
     MOVE     WK-HEAD14        TO      HD04-BIN.
*    値付
     MOVE     WK-HEAD15        TO      HD04-NETUKE.
     EVALUATE WK-HEAD15
         WHEN 1     MOVE NC"値付必要" TO   HD04-NETUKENM
         WHEN 2     MOVE NC"値付不要" TO   HD04-NETUKENM
         WHEN OTHER MOVE NC"＊＊＊＊" TO   HD04-NETUKENM
     END-EVALUATE.
*    特売
     MOVE     WK-HEAD16        TO      HD04-TOKUBAI.
     EVALUATE WK-HEAD16
         WHEN 0     MOVE NC"通常" TO   HD04-TOKUBAINM
         WHEN 1     MOVE NC"特売" TO   HD04-TOKUBAINM
         WHEN OTHER MOVE NC"＊＊" TO   HD04-TOKUBAINM
     END-EVALUATE.
*    発注
     MOVE     WK-HEAD17        TO      HD04-HATYU.
     EVALUATE WK-HEAD17
         WHEN 0     MOVE NC"通常" TO   HD04-HATYUNM
         WHEN 1     MOVE NC"新店" TO   HD04-HATYUNM
         WHEN 2     MOVE NC"改装" TO   HD04-HATYUNM
         WHEN OTHER MOVE NC"＊＊" TO   HD04-HATYUNM
     END-EVALUATE.
*    部門区分
     MOVE     WK-HEAD18        TO      HD04-BUMONKBN.
*    梱包店舗
     MOVE     WK-HEAD10        TO      HD04-KTENCD.
     MOVE     WK-HEAD21        TO      HD04-KTENNM.
*    伝票仕分
     MOVE     WK-HEAD19        TO      HD05-SIWAKE.
*    摘要
     MOVE     WK-HEAD22        TO      HD05-TEKIYO.
*    計上日
     MOVE     WK-HEAD24        TO      HD05-KEIJYO.
*    検収担当者
     MOVE     WK-HEAD25        TO      HD05-TANTO.
*
     IF       PAGE-SW   =    0
              IF        CNT-PAGE       >    ZERO
                        MOVE      SPACE     TO   PRT-REC
                        WRITE     PRT-REC   AFTER     PAGE
              END-IF
*             頁セット
              ADD       1         TO        CNT-PAGE
              MOVE      CNT-PAGE  TO        PG-CNT
              WRITE     PRT-REC   FROM      HEAD-01   AFTER  2
              WRITE     PRT-REC   FROM      HEAD-02   AFTER  2
              MOVE      1         TO        PAGE-SW
              MOVE      4         TO        L-CNT
     ELSE
              MOVE      SPACE     TO        PRT-REC
              WRITE     PRT-REC                       AFTER  2
              MOVE      ZERO      TO        PAGE-SW
              MOVE      2         TO        L-CNT
     END-IF.
     WRITE    PRT-REC          FROM   SEN1       AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-04    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-05    AFTER    1.
     WRITE    PRT-REC          FROM   SEN2       AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-06    AFTER    1.
*    行カウンタ
     MOVE     6                TO     L-CNT.
*    明細カウンター初期化
     MOVE     ZERO             TO     MEISAI-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TAIL-01.
*    発注数量計
     MOVE   GK-HATYU             TO   TL01-GHATYU.
*    検収数量計
     MOVE   "("                  TO   TL01-GKENSYU-0.
     MOVE   GK-KENSYU            TO   TL01-GKENSYU.
     MOVE   ")"                  TO   TL01-GKENSYU-1.
*    原価金額合計
     MOVE   GK-GENKAKEI          TO   TL01-GGENKIN.
*    検収数量計
     MOVE   "("                  TO   TL01-GKENKIN-0.
     MOVE   GK-KENSYUKEI         TO   TL01-GKENKIN.
     MOVE   ")"                  TO   TL01-GKENKIN-1.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     19  -  MEISAI-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    CVCSG001  DSPFILE.
*
 END-EXIT.
     EXIT.
