****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　共通システム                      *
*    業務名　　　　　　　：　手書伝票発行　　　　　　　        *
*    モジュール名　　　　：　共通手書伝票発行（ＴＡ_型）
*    作成日／作成者　　　：　99/09/29  /HAGIWARA               *
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　：                                    *
*    再利用ＰＧ　　　　　：　VDA2200B.SKTSLIB                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　11/10/07 /YOSHIDA.M               *
*    更新概要　　　　　　：　基幹サーバ統合　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1002B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/09/28.
****************************************************************
*                                                              *
 ENVIRONMENT            DIVISION.
*                                                              *
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
            YA     IS   YA.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    伝票データ
     SELECT   SHTDENF            ASSIGN    TO        JHTTEGL1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   SEQUENTIAL
                                 RECORD    KEY  IS   DEN-F01
                                                     DEN-F02
                                                     DEN-F04
                                                     DEN-F051
***2011.10.07(DEN-F07,DEN-F112)
                                                     DEN-F07
                                                     DEN-F112
                                                     DEN-F03
                                 FILE STATUS    IS   DEN-ST.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO     LP-04-PRTF.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データ＞*****
 FD  SHTDENF
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FKY10021   OF   XMDLIB.
*
*----<< 取引先マスタ >>-*
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   30   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
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
     03  CHK-FLG                  PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)V9   VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
*    03  W-SURYO                  PIC  9(06)V9   VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
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
         05  DENNO1               PIC  9(08)     VALUE   ZERO.
*        05  SINSEINO             PIC  9(08)     VALUE   ZERO.
     03  DENPYO-END               PIC  9(01)     VALUE   ZERO.
     03  OLD-F02                  PIC  9(09)     VALUE   ZERO.
     03  NEW-F02                  PIC  9(09)     VALUE   ZERO.
     03  WK-BIKO1                 PIC  X(15)     VALUE   SPACE.
     03  WK-BIKO2                 PIC  X(15)     VALUE   SPACE.
     03  WK-TOKKN                 PIC  X(15)     VALUE   SPACE.
*日付
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付編集
 01  HEN-DATE.
     03  HEN-YYYY                 PIC   9(04)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  HEN-MM                   PIC   9(02)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  HEN-DD                   PIC   9(02)  VALUE  ZERO.
***  ﾊｯﾁｭｳ ﾋﾞ
 01  WK-DEN111                    PIC  9(08).
 01  WK-DEN111R    REDEFINES      WK-DEN111.
     03  WK-DEN111YY              PIC  9(02).
     03  WK-DEN111Y               PIC  9(02).
     03  WK-DEN111M               PIC  9(02).
     03  WK-DEN111D               PIC  9(02).
***  ﾉｳﾋﾝ ﾋﾞ
 01  WK-DEN112                    PIC  9(08).
 01  WK-DEN112R    REDEFINES      WK-DEN112.
     03  WK-DEN112YY              PIC  9(02).
     03  WK-DEN112Y               PIC  9(02).
     03  WK-DEN112M               PIC  9(02).
     03  WK-DEN112D               PIC  9(02).
***  ｽｳﾘｮｳ
 01  WK-DEN15                     PIC S9(09)V9.
 01  WK-DEN15R     REDEFINES      WK-DEN15.
     03  WK-DEN15A                PIC S9(09).
     03  WK-DEN15B                PIC  9(01).
***  ｹﾞﾝｶ ﾀﾝｶ
 01  WK-DEN172                    PIC S9(09)V99.
 01  WK-DEN172R    REDEFINES      WK-DEN172.
     03  WK-DEN172A               PIC S9(09).
     03  WK-DEN172B               PIC  9(02).
***  ﾃﾞﾝﾋﾟｮｳﾊﾞﾝｺﾞｳﾍﾝｼｭｳ
 01  WK-DENPYO.
     03  WK-DENPYO-1              PIC  X(02)     VALUE   SPACE.
     03  WK-DENPYO-2              PIC  X(09)     VALUE   SPACE.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中。".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです。".
         05  MSG05                PIC  N(30)     VALUE
         NC"取引先コードを入力してください。".
         05  MSG06                PIC  N(30)     VALUE
         NC"取引先Ｍに存在しません。取引先Ｍを登録して下さい。".
         05  MSG07                PIC  N(30)     VALUE
         NC"指定された取引先の伝票データが存在しません。".
         05  MSG08                PIC  N(30)     VALUE
         NC"伝票枚数カウント中！！".
*## 1999/11/02 NAV T.T START ##*
         05  MSG09.
             07  FILLER           PIC  N(15)     VALUE
                 NC"入力した取引先は、ＴＡ_型出力".
             07  FILLER           PIC  N(15)     VALUE
                 NC"対象の取引先ではありません。　".
*## 1999/11/02 NAV T.T END   ##*
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     9      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(14)     VALUE
         NC"印刷を続けますか？　Ｙ／Ｎ".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"得意先マスタ　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HD1.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  HD1-01                   PIC  X(30).
*
 01  HD2                          CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD2-01                   PIC  X(20).
     03  FILLER                   PIC  X(45)     VALUE   SPACE.
     03  HD2-02                   PIC  N(10).
*
 01  HD3.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD3-01                   PIC  X(20)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD3-02                   PIC  X(12).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-03                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-04                   PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-05                   PIC  X(11).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-06                   PIC  X(06).
     03  FILLER                   PIC  X(26)     VALUE   SPACE.
     03  HD3-07                   PIC  9(06).
     03  HD3-08                   PIC  9(06).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD3-09                   PIC  X.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
*
*    ﾒｲｻｲ
 01  DT1.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  DT1-01                   PIC  X(25).
*
 01  DT2.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  DT2-01                   PIC  X(25).
     03  DT2-02                   PIC  X(13).
     03  DT2-03                   PIC  ZZZZZZZ.
     03  DT2-04                   PIC  ZZZZZ.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  DT2-051                  PIC  ZZZZ9.
     03  DT2-052                  PIC  Z.
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  DT2-061                  PIC  ZZZZZ9.
     03  DT2-062                  PIC  ZZ.
     03  DT2-07                   PIC  ZZZZZZZZ9.
     03  DT2-08                   PIC  ZZZZZ9.
     03  DT2-09                   PIC  ZZZZZZZZ9.
*
*    ｺﾞｳｹｲ
 01  TL1.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  FILLER                   PIC  X(79)     VALUE   SPACE.
     03  TL1-01                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-02                   PIC  ZZZZZZZZ9.
 01  TL2.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  TL2-01                   PIC  X(15).
 01  TL3.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  TL3-01                   PIC  X(15).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
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
     USE AFTER          EXCEPTION      PROCEDURE      PRINTF.
     DISPLAY  FILE-ERR010      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ﾃﾞﾝﾋﾟｮｳ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      SHTDENF.
     DISPLAY  FILE-ERR030      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ｶﾞﾒﾝ    ｴﾗ- >> ---*
 000-DSPFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR040      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
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
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       DSPFILE.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DD.
*    ワーク初期化
     MOVE     ZERO               TO   DENNO1.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FKY10021.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*****画面の初期化*****
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FKY10021.
     MOVE          ZERO      TO   WRK-MAI.
 MAIN010.
*****画面表示*****
     PERFORM       DSP-WRITE-SEC.
*****取引先入力*****
     MOVE         "TKCDR"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*****アテンション判定*****
     EVALUATE      DSP-FNC
         WHEN     "F004"
                   GO                  TO   MAIN-EXIT
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*****取引先入力チェック*****
     IF   TOKCD    NOT   NUMERIC
     OR   TOKCD    =     ZERO
          MOVE     5    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN010
     ELSE
*****取引先マスタ検索*****
          MOVE     TOKCD     TO   TOK-F01
          READ     HTOKMS
            INVALID
                   MOVE     6    TO   ERR-FLG
                   PERFORM       ERR-MSG-SEC
                   GO       TO   MAIN010
            NOT  INVALID
                   MOVE  TOK-F03 TO   TOKNM
                   MOVE  TOK-F04 TO   WK-TOKKN
*## 1999/11/02 NAV T.T START ##*
                   IF    TOK-F79 NOT = 2
                         MOVE    9  TO  ERR-FLG
                         PERFORM ERR-MSG-SEC
                         GO         TO  MAIN010
                   END-IF
*## 1999/11/02 NAV T.T END   ##*
          END-READ
     END-IF.
     CLOSE    SHTDENF.
     OPEN     INPUT  SHTDENF.
*****伝票枚数カウント*****
     MOVE     ZERO         TO    WRK-MAI.
***2011.10.07 ST
     MOVE     SPACE        TO    DEN-REC.
     INITIALIZE                  DEN-REC.
***2011.10.07 EN
     MOVE     TOKCD        TO    DEN-F01.
     MOVE     ZERO         TO    DEN-F02  DEN-F04.
     MOVE     ZERO         TO    DEN-F051 DEN-F03.
*****売上ファイルスタート*****
***2011.10.07 ST
***  START  SHTDENF  KEY  IS >=  DEN-F01 DEN-F02 DEN-F04
***                              DEN-F051 DEN-F03
     START  SHTDENF  KEY  IS >=  DEN-F01 DEN-F02 DEN-F04
                                 DEN-F051 DEN-F07 DEN-F112
                                 DEN-F03
***2011.10.07 EN
          INVALID  KEY
              MOVE    7         TO        ERR-FLG
              PERFORM           ERR-MSG-SEC
              GO      TO        MAIN010
     END-START.
*****DISPLAY "TOKCD1 = " TOKCD UPON CONS.
     MOVE    8         TO        ERR-FLG.
     PERFORM           ERR-MSG-SEC.
     MOVE    ZERO      TO        ERR-FLG.
     PERFORM           DSP-WRITE-SEC.
*****伝票枚数カウントフラグクリア
     MOVE    ZERO      TO        DENPYO-END.
     MOVE    ZERO      TO        WRK-MAI.
*****指定取引先手書伝票枚数カウント*****
     PERFORM  DEN-CNT-SEC  UNTIL  DENPYO-END = 9.
*****伝票枚数チェック*****
     IF       WRK-MAI       =      ZERO
              MOVE    7         TO        ERR-FLG
              PERFORM           ERR-MSG-SEC
              GO      TO        MAIN010
     ELSE
              MOVE    SPACE     TO        MD05
     END-IF.
 MAIN020.
*****画面表示*****
     PERFORM       DSP-WRITE-SEC.
*****キー入力*****
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*****アテンション判定*****
     EVALUATE      DSP-FNC
         WHEN     "F004"
                   GO                  TO   MAIN-EXIT
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*****処理対象未入力時処理*****
     IF   MD04  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN020
     ELSE
          MOVE  MD04    TO   WRK-R040
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*****プリントファイルのＯＰＥＮ*****
     OPEN     OUTPUT    PRINTF.
*****処理判定*****
     EVALUATE      MD04
*********テストプリント*********
         WHEN      1
                   PERFORM   TEST-PRT-SEC
*********全プリント*********
         WHEN      2
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     SHTDENF
                   OPEN      INPUT     SHTDENF
***2011.10.07 ST
                   MOVE     SPACE        TO    DEN-REC
                   INITIALIZE                  DEN-REC
***2011.10.07 EN
                   MOVE     TOKCD        TO    DEN-F01
                   MOVE     ZERO         TO    DEN-F02
                   MOVE     ZERO         TO    DEN-F04
                                         DEN-F051
                                         DEN-F03
*
                   START    SHTDENF       KEY  IS >=
***2011.10.07 ST
***                 DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F03
                    DEN-F01  DEN-F02 DEN-F04 DEN-F051 DEN-F07
                    DEN-F112 DEN-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
*********範囲指定*********
         WHEN      3
                   PERFORM   KEY-IN-SEC
                   IF        END-FLG   =   "END"
                             GO        TO   MAIN-EXIT
                   END-IF
                   IF        CHK-FLG   =   "CHK"
                             GO        TO   MAIN-EXIT
                   END-IF
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   CLOSE     SHTDENF
                   OPEN      INPUT     SHTDENF
***2011.10.07 ST
                   MOVE     SPACE        TO    DEN-REC
                   INITIALIZE                  DEN-REC
***2011.10.07 EN
                   MOVE     TOKCD        TO    DEN-F01
                   MOVE     ZERO         TO    DEN-F02
                   MOVE     ZERO         TO    DEN-F04
                                         DEN-F051
                                         DEN-F03
*
                   START    SHTDENF       KEY  IS >=
***2011.10.07 ST
***                 DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F03
                    DEN-F01  DEN-F02 DEN-F04 DEN-F051 DEN-F07
                    DEN-F112 DEN-F03
***2011.10.07 EN
                   END-START
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
*****プリントファイルＣＬＯＳＥ*****
     CLOSE         PRINTF    SHTDENF.
*****次対象の入力為，ＲＥＡＤスイッチＯＦＦ*****
     MOVE          ZERO      TO       RD-SW.
*****伝票データＦＯＰＥＮ*****
     OPEN          INPUT     SHTDENF.
*****伝票_ブレイクキーの初期化
     MOVE          ZERO      TO       NEW-F02.
     MOVE          ZERO      TO       OLD-F02.
     MOVE          SPACE     TO       SYORI-FLG.
     MOVE          ZERO      TO       DENPYO-END.
*****処理種別＝１の場合は，処理種別再入力へ
     IF  MD04  =  1
         GO        TO        MAIN020
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                       伝票_カウント                1.1
****************************************************************
 DEN-CNT-SEC               SECTION.
*
*****DISPLAY "TOKCD2 = " TOKCD UPON CONS.
     MOVE     NEW-F02      TO   OLD-F02.
*
     READ     SHTDENF
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*    取引先コードブレイクチェック
     IF       DEN-F01      >    TOKCD
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*
     MOVE     DEN-F02  TO        NEW-F02.
*
     IF   OLD-F02      NOT =     NEW-F02
          ADD       1  TO        WRK-MAI
     END-IF.
*****DISPLAY "WRK-MAI = " WRK-MAI UPON CONS.
*
 DEN-CNT-EXIT.
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
     MOVE     HEN-DATE  TO        SDATE.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FKY10021" TO        DSP-FMT.
*    画面表示
     WRITE    FKY10021.
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
     MOVE          SPACE     TO   CHK-FLG.
*    画面読込み
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
*        取消
         WHEN     "F004"
                   MOVE     "CHK"      TO   CHK-FLG
                   GO                  TO   KEY-IN-EXIT
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
*    伝票_のブレイク判定
     IF       DEN-F02   NOT =     DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      DEN-F02   TO   DENNO1
*             MOVE      DEN-F     TO   SINSEINO
     END-IF.
*    行カウンタの判定
     IF       L-CNT        >=     25
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
     END-IF.
*    項目初期化
     INITIALIZE                  DT1
                                 DT2.
*商品名１
     MOVE        DEN-F1421 TO        DT1-01.
*商品名２
     MOVE        DEN-F1422 TO        DT2-01.
*商品コード
     MOVE        DEN-F25   TO        DT2-02.
*数量
     MOVE        DEN-F15   TO        WK-DEN15.
     MOVE        WK-DEN15A TO        DT2-051.
*少数点編集
     IF   WK-DEN15B  NOT = ZERO
       MOVE      WK-DEN15B TO        DT2-052
     END-IF.
*原価単価（編集）
     MOVE        DEN-F172  TO        WK-DEN172.
     MOVE        WK-DEN172A  TO      DT2-061.
     MOVE        WK-DEN172B  TO      DT2-062.
*売価単価（編集）
     MOVE        DEN-F173  TO        DT2-08.
*原価金額計算
     COMPUTE     W-GENKA  =  DEN-F15  *  DEN-F172.
     MOVE        W-GENKA   TO        DT2-07.
*売価金額計算
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-BAIKA   TO        DT2-09.
*合計エリア加算
     ADD         W-GENKA   TO        G-GENKA.
     ADD         W-BAIKA   TO        G-BAIKA.
*
     WRITE    PRT-REC    FROM    DT1    AFTER    1.
     WRITE    PRT-REC    FROM    DT2    AFTER    1.
     INITIALIZE          DT1   DT2.
     ADD         2         TO        L-CNT.
*
     PERFORM  FL-READ-SEC.
*
     IF       SYORI-FLG   =        "END"
              PERFORM   TAIL-WT-SEC
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
     READ     SHTDENF   AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-READ.
     IF       DEN-F01   >   TOKCD
              GO                  TO   READ-000
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-IF.
     IF       DEN-F03 >    20
**************備考欄の退避（退避されている伝票_と同一の場合）
              IF  DEN-F03  =  80
*### 1999/04/02 NAV TKAHASHI START ###*
              AND DEN-F02  =  DENNO1
*### 1999/04/02 NAV TKAHASHI END   ###*
                  MOVE     DEN-F1421  TO  WK-BIKO1
                  MOVE     DEN-F1422  TO  WK-BIKO2
              END-IF
              GO                  TO   READ-000
     END-IF.
*    全プリント時判定
**** IF       MD04    =    2
****          GO                  TO   READ-010
**** END-IF.
*    伝票番号範囲指定大小チェック
     IF       DEN-F02 <    MD02
              GO                  TO   READ-000
     END-IF.
     IF       DEN-F02 >    MD03
              GO                  TO   READ-000
     END-IF.
     IF       DEN-F02 <    MD02
              GO                  TO   READ-000
     END-IF.
 READ-010.
*    １件目時処理
     IF       RD-SW     =    0
              MOVE      DEN-F02   TO   DENNO1
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*    ヘッド部項目初期化
     INITIALIZE                  HD1 HD2 HD3.

*    MOVE        SPACE     TO        HD1-01.
*    MOVE        "@"       TO        HD1-01.
*発注日
     MOVE        DEN-F111  TO        HD3-07.
*納品日
     MOVE        DEN-F112  TO        HD3-08.
*店舗コード
     MOVE        DEN-F07   TO        HD3-02.
*分類コード
     MOVE        DEN-F12   TO        HD3-03.
*伝票区分
     MOVE        DEN-F132  TO        HD3-04.
*伝票番号
     MOVE        SPACE     TO        WK-DENPYO-1.
     MOVE        DEN-F02   TO        WK-DENPYO-2.
     MOVE        WK-DENPYO TO        HD3-05.
*取引先コード
     MOVE        DEN-F01(3:6)  TO    HD3-06.
*#2021/03/05 NAV ST 取引先ＣＤ変換
     EVALUATE DEN-F01
         WHEN   87371    MOVE "  8737"     TO   HD3-06
         WHEN   87372    MOVE "  8737"     TO   HD3-06
         WHEN   87373    MOVE "  8737"     TO   HD3-06
         WHEN   87374    MOVE "  8737"     TO   HD3-06
         WHEN   87375    MOVE "  8737"     TO   HD3-06
         WHEN   87376    MOVE "  8737"     TO   HD3-06
         WHEN   883      MOVE "   880"     TO   HD3-06
         WHEN   882      MOVE "   880"     TO   HD3-06
         WHEN   14272    MOVE "  1427"     TO   HD3-06
         WHEN   14273    MOVE "  1427"     TO   HD3-06
         WHEN   100442   MOVE "100441"     TO   HD3-06
         WHEN   100428   MOVE "100427"     TO   HD3-06
         WHEN   139381   MOVE " 13938"     TO   HD3-06
         WHEN   171371   MOVE " 17137"     TO   HD3-06
         WHEN   1731     MOVE "   173"     TO   HD3-06
         WHEN   1732     MOVE "   173"     TO   HD3-06
         WHEN   7601     MOVE "   760"     TO   HD3-06
         WHEN   7602     MOVE "   760"     TO   HD3-06
         WHEN   12251    MOVE "  1225"     TO   HD3-06
         WHEN   12252    MOVE "  1225"     TO   HD3-06
         WHEN   OTHER    MOVE  DEN-F01(3:6) TO  HD3-06
     END-EVALUATE.
*#2021/03/05 NAV ED 取引先ＣＤ変換
*社名
     MOVE        WK-TOKKN  TO        HD2-01.
*取引先名
     MOVE NC"_　サカタのタネ"   TO  HD2-02.
*店舗名（カナ）
     MOVE        DEN-F30   TO        HD3-01.
*ヘッダ印字
     WRITE  PRT-REC  FROM  HD1  AFTER  2.
     WRITE  PRT-REC  FROM  HD2  AFTER  2.
     WRITE  PRT-REC  FROM  HD3  AFTER  1.
     MOVE   SPACE          TO       PRT-REC.
     WRITE  PRT-REC  AFTER 1.
*行カウント
     ADD         7         TO        L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TL1  TL2  TL3.
*原価金額合計
     MOVE        G-GENKA   TO        TL1-01.
*売価金額合計
     MOVE        G-BAIKA   TO        TL1-02.
*合計行印字調整
     COMPUTE     CNT-AFTER   =   27   -   L-CNT.
*備考１
     MOVE        WK-BIKO1  TO        TL2-01.
*備考２
     MOVE        WK-BIKO2  TO        TL3-01.
*合計行出力
     WRITE  PRT-REC  FROM  TL1  AFTER  CNT-AFTER.
*備考出力
     WRITE  PRT-REC  FROM  TL2  AFTER  2.
     WRITE  PRT-REC  FROM  TL3  AFTER  1.
*
     MOVE        SPACE     TO        PRT-REC.
     WRITE       PRT-REC   AFTER     PAGE.
*ワーク合計計算エリア
     MOVE        ZERO      TO        G-GENKA  G-BAIKA
                                     L-CNT.
     MOVE        SPACE     TO        WK-BIKO1  WK-BIKO2.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*テスト印字文字セット
     MOVE   ALL "9"              TO   HD3-05  HD3-07
                                      HD3-08  HD3-09.
     MOVE   ALL "9"              TO   DT2-03  DT2-04   DT2-07
                                      DT2-051 DT2-061
                                      DT2-052 DT2-062
                                      DT2-08  DT2-09.
     MOVE   ALL "9"              TO   TL1-01  TL1-02.
     MOVE   ALL "*"              TO   DT1-01  DT2-02   DT2-01
                                      HD1-01  HD2-01
                                      HD3-01  HD3-02   HD3-03
                                      HD3-04  HD3-06
                                      TL2-01  TL3-01.
*取引先名
     MOVE NC"_　サカタのタネ"   TO   HD2-02.
*ヘッダ印字
     WRITE  PRT-REC  FROM  HD1  AFTER  2.
     WRITE  PRT-REC  FROM  HD2  AFTER  2.
     WRITE  PRT-REC  FROM  HD3  AFTER  1.
     MOVE   SPACE          TO       PRT-REC.
     WRITE  PRT-REC  AFTER 1.
*明細印字
     PERFORM VARYING L-CNT FROM 1 BY 1 UNTIL L-CNT > 9
             WRITE  PRT-REC  FROM  DT1  AFTER  1
             WRITE  PRT-REC  FROM  DT2  AFTER  1
     END-PERFORM.
**** WRITE  PRT-REC  AFTER PAGE.
*テイル印字
     WRITE  PRT-REC  FROM  TL1  AFTER  2.
     WRITE  PRT-REC  FROM  TL2  AFTER  2.
     WRITE  PRT-REC  FROM  TL3  AFTER  1.
     MOVE   SPACE          TO       PRT-REC.
     WRITE  PRT-REC  AFTER PAGE.
*
     INITIALIZE      HD1  HD2  HD3  DT1
                     DT2  TL1  TL2  TL3.
     MOVE   ZERO           TO       L-CNT.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    IF       WRK-R040   NOT  =    3
*             DISPLAY   DENPYO    UPON      CONS
*             ACCEPT    AAA       FROM      CONS
*    END-IF.
*    ファイルのＣＬＯＳＥ
     CLOSE    SHTDENF   DSPFILE   HTOKMS.
*
 END-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  SKY1002B.
******************************************************************
