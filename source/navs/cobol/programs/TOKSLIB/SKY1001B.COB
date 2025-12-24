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
*    更新日／更新者　　　：　21/03/05 /NAV TAKAHASHI           *
*    更新概要　　　　　　：　取引先ＣＤ対応　　　　　　　　　　*
*    更新日／更新者　　　：　24/01/12 /NAV TAKAHASHI           *
*    更新概要　　　　　　：　取引先ＣＤ対応　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1001B.
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
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
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
     COPY    FKY10011   OF   XMDLIB.
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
     03  WK-F111                  PIC  9(08)     VALUE   ZERO.
     03  WK-F112                  PIC  9(08)     VALUE   ZERO.
     03  WK-F172                  PIC  9(09)V99  VALUE   ZERO.
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
 01  HD0.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD0-01                   PIC  X(20).
*
 01  HD1   CHARACTER          TYPE IS        YA.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  HD1-01                   PIC  X(10).
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  HD1-02                   PIC  ZZZZ9.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  HD1-03                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD1-04                   PIC  X(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD1-05                   PIC  ZZZZZZZZ9.
*#2024/01/12 NAV ST
*****03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD1-06                   PIC  ZZZZZ9.
*****03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
*#2024/01/12 NAV ED
     03  HD1-061                  PIC  N(10).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD1-07                   PIC  ZZZZZZ.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD1-08                   PIC  ZZZZZZ.
     03  FILLER                   PIC  X(31)     VALUE   SPACE.
*
*    ﾒｲｻｲ
 01  DT1.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  DT1-01                   PIC  X(15).
     03  FILLER                   PIC  X(114)    VALUE   SPACE.
*
 01  DT2.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  DT2-01                   PIC  X(15).
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
     03  DT2-02                   PIC  X(13).
*#2021/08/31 NAV ST
*****03  FILLER                   PIC  X(16)     VALUE   SPACE.
*****03  DT2-03                   PIC  ZZZ9.
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  DT2-03                   PIC  ZZZZ9.
*#2021/08/31 NAV ED
     03  FILLER                   PIC  X(14)     VALUE   SPACE.
     03  DT2-04                   PIC  ZZZZZ9.
     03  DT2-05                   PIC  ZZ.
     03  DT2-06                   PIC  ZZZZZZZZ9.
     03  DT2-07                   PIC  ZZZZZ9.
     03  DT2-08                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(27)     VALUE   SPACE.
*
*    ｺﾞｳｹｲ
 01  TL1.
     03  FILLER                   PIC  X(85)     VALUE   SPACE.
     03  TL1-01                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-02                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(27)     VALUE   SPACE.
 01  TL2.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  TL2-01                   PIC  X(15).
 01  TL3.
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  TL3-01                   PIC  X(15).
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
     MOVE     SPACE              TO   FKY10011.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*****画面の初期化*****
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FKY10011.
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
          READ     HTOKMS    INVALID
                   MOVE     6    TO   ERR-FLG
                   PERFORM       ERR-MSG-SEC
                   GO       TO   MAIN010
                   NOT  INVALID
                   MOVE  TOK-F03 TO   TOKNM
                   MOVE  TOK-F04 TO   WK-TOKKN
*## 1999/11/02 NAV T.T START ##*
                   IF    TOK-F79 NOT = 1
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
**************DISPLAY "AAAAAAA" UPON CONS
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
*****DISPLAY "TOKCD3 = " TOKCD UPON CONS.
*****伝票枚数チェック*****
     IF       WRK-MAI       =      ZERO
**************DISPLAY "BBBBBBB" UPON CONS
**************DISPLAY "WRK-MAI = " WRK-MAI UPON CONS
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
*****DISPLAY "DEN-F01 = " DEN-F01  UPON CONS.
*****DISPLAY "TOKCD R = " TOKCD    UPON CONS.
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
     MOVE    "FKY10011" TO        DSP-FMT.
*    画面表示
     WRITE    FKY10011.
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
     IF       L-CNT        >=     23
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
     END-IF.
*    項目初期化
     INITIALIZE                  DT1
                                 DT2.
*    明細項目セット
*    商品名（１）
     MOVE     DEN-F1421          TO   DT1-01.
*    商品名（２）
     MOVE     DEN-F1422          TO   DT2-01.
*    取引先商品コード
     MOVE     DEN-F25            TO   DT2-02.
*    数量
     MOVE     DEN-F15            TO   DT2-03.
*    原価単価
     MOVE     DEN-F172           TO   WK-F172.
     MOVE     WK-F172(1:9)       TO   DT2-04.
     MOVE     WK-F172(10:2)      TO   DT2-05.
*    原価金額
     COMPUTE  W-GENKA   =    DEN-F15  *  DEN-F172
     IF       W-GENKA(10:1)  >  4
              ADD  1             TO   W-GENKA
     END-IF.
     MOVE     W-GENKA            TO   DT2-06.
     COMPUTE  G-GENKA   =    G-GENKA  +  W-GENKA.
*    売価単価
     MOVE     DEN-F173           TO   DT2-07.
*    売価金額
     COMPUTE  W-BAIKA   =    DEN-F15  *  DEN-F173
     MOVE     W-BAIKA            TO   DT2-08.
     COMPUTE  G-BAIKA   =    G-BAIKA  +  W-BAIKA.
*
     WRITE    PRT-REC        FROM     DT1   AFTER     1.
     WRITE    PRT-REC        FROM     DT2   AFTER     1.
*
     ADD      2                  TO   L-CNT.
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
     INITIALIZE                  HD0.
     INITIALIZE                  HD1.
*--- 項目セット
*    社名
     MOVE     WK-TOKKN           TO    HD0-01.
*    店舗名称
     MOVE     DEN-F30            TO    HD1-01.
*    店舗コード
     MOVE     DEN-F07            TO    HD1-02.
*    分類コード
     MOVE     DEN-F12            TO    HD1-03.
*    伝票区分
     MOVE     DEN-F132           TO    HD1-04.
*    伝票_
     MOVE     DEN-F02            TO    HD1-05.
*    取引先コード
     MOVE     DEN-F01            TO    HD1-06.
*#2021/03/05 NAV ST 取引先ＣＤ変換
     EVALUATE DEN-F01
         WHEN   87371    MOVE  8737      TO   HD1-06
         WHEN   87372    MOVE  8737      TO   HD1-06
         WHEN   87373    MOVE  8737      TO   HD1-06
         WHEN   87374    MOVE  8737      TO   HD1-06
         WHEN   87375    MOVE  8737      TO   HD1-06
         WHEN   87376    MOVE  8737      TO   HD1-06
         WHEN   880      MOVE  56        TO   HD1-06
         WHEN   883      MOVE  56        TO   HD1-06
         WHEN   882      MOVE  56        TO   HD1-06
         WHEN   1427     MOVE  250       TO   HD1-06
         WHEN   14272    MOVE  250       TO   HD1-06
         WHEN   14273    MOVE  250       TO   HD1-06
         WHEN   100441   MOVE  56        TO   HD1-06
         WHEN   100442   MOVE  56        TO   HD1-06
         WHEN   100427   MOVE  250       TO   HD1-06
         WHEN   100428   MOVE  250       TO   HD1-06
         WHEN   13938    MOVE  56        TO   HD1-06
         WHEN   139381   MOVE  56        TO   HD1-06
         WHEN   17137    MOVE  250       TO   HD1-06
         WHEN   171371   MOVE  250       TO   HD1-06
         WHEN   1731     MOVE  173       TO   HD1-06
         WHEN   1732     MOVE  173       TO   HD1-06
         WHEN   7601     MOVE  760       TO   HD1-06
         WHEN   7602     MOVE  760       TO   HD1-06
         WHEN   12251    MOVE  1225      TO   HD1-06
         WHEN   12252    MOVE  1225      TO   HD1-06
         WHEN   OTHER    MOVE  DEN-F01   TO   HD1-06
     END-EVALUATE.
*#2021/03/05 NAV ED 取引先ＣＤ変換
     MOVE     SPACE              TO    HD1-061.
     MOVE     NC"_　サカタのタネ　　" TO HD1-061.
*    発注日
     MOVE     DEN-F111           TO    WK-F111.
     MOVE     WK-F111(3:6)       TO    HD1-07.
*    納品日
     MOVE     DEN-F112           TO    WK-F112.
     MOVE     WK-F112(3:6)       TO    HD1-08.
*
     WRITE    PRT-REC          FROM   HD0    AFTER    6.
     WRITE    PRT-REC          FROM   HD1    AFTER    2.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*    行カウンタ
     MOVE     11               TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TL1  TL2  TL3.
*    原価金額合計
     MOVE   G-GENKA              TO   TL1-01.
*    売価金額合計
     MOVE   G-BAIKA              TO   TL1-02.
*    備考１
     MOVE   WK-BIKO1             TO   TL2-01.
*    備考２
     MOVE   WK-BIKO2             TO   TL3-01.
*    仕入申請書_
*****MOVE   SINSEINO             TO   TL03.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     25  -  L-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TL1   AFTER   CNT-AFTER.
*    備考印字
     WRITE    PRT-REC          FROM   TL2   AFTER   2.
     WRITE    PRT-REC          FROM   TL3   AFTER   1.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*    ワーク初期化
     MOVE     ZERO             TO     G-GENKA
                                      G-BAIKA.
     MOVE     SPACE            TO     WK-BIKO1
                                      WK-BIKO2.
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*    テスト印字項目セット
     MOVE   ALL "9"              TO   HD1-02  HD1-05  HD1-06
                                      HD1-07  HD1-08
                                      DT2-03  DT2-04  DT2-05
                                      DT2-06  DT2-07  DT2-08
                                      TL1-01  TL1-02
     MOVE   ALL "*"              TO   HD1-01  HD1-03  HD1-04
                                      DT1-01  DT2-01  DT2-02
                                      TL2-01  TL3-01.
     MOVE   NC"_　サカタのタネ" TO   HD1-061.
*    ヘッド印字
     WRITE    PRT-REC          FROM   HD0    AFTER    6.
     WRITE    PRT-REC          FROM   HD1    AFTER    2.
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  3.
*    明細印字
     PERFORM   VARYING   L-CNT      FROM  1  BY  1
                                        UNTIL   L-CNT     >  6
              WRITE  PRT-REC   FROM   DT1   AFTER     1
              WRITE  PRT-REC   FROM   DT2   AFTER     1
     END-PERFORM.
*    テイル印字
     WRITE    PRT-REC          FROM   TL1   AFTER     2.
     WRITE    PRT-REC          FROM   TL2   AFTER     2.
     WRITE    PRT-REC          FROM   TL3   AFTER     1.
*
     MOVE     SPACE            TO     PRT-REC.
*****WRITE    PRT-REC          AFTER  PAGE.
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
 END PROGRAM  SKY1001B.
******************************************************************
