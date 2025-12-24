# STE2401B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE2401B.COB`

## ソースコード

```cobol
***********************************************************
*   顧客名　　    ：   _サカタのタネ殿
*   システム名    ：   販売管理システム
*   サブシステム名：   手書伝票発行　
*   プログラム名　：   ハンズマン手書伝票発行　　　
*   プログラムID  ：   STE2401B
*   作成者        ：   ナブ・アシスト　上村
*   作成日        ：   1995.05.25
*   更新履歴      ：
*     2011/10/05 飯田/NAV 基幹サーバ統合
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE2401B.
 AUTHOR.                K.KAMIMURA.
 DATE-WRITTEN.          95/05/25.
******************************************************************
 ENVIRONMENT            DIVISION.
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票データ
     SELECT   HKYOTU1        ASSIGN    TO        DA-01-VI-JHTTEGL1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   SEQUENTIAL
                             RECORD    KEY  IS   KYO-F01
                                                 KYO-F02
                                                 KYO-F04
                                                 KYO-F051
                                       *> 2011/10/05,S  S.I/NAV
                                                 KYO-F07
                                                 KYO-F112
                                       *> 2011/10/05,E  S.I/NAV
                                                 KYO-F03
                             FILE STATUS    IS   KYO-ST.
*取引先マスタ
*    SELECT   HTOKMS         ASSIGN    TO        DA-01-VI-TOKMS2
*                            ORGANIZATION   IS   INDEXED
*                            ACCESS    MODE IS   RANDOM
*                            RECORD    KEY  IS   TOK-F01
*                            FILE STATUS    IS   TOK-ST.
*条件ファイル
*    SELECT   HJYOKEN        ASSIGN    TO        DA-01-VI-JYOKEN1
*                            ORGANIZATION   IS   INDEXED
*                            ACCESS    MODE IS   DYNAMIC
*                            RECORD    KEY  IS   JYO-F01
*                                                JYO-F02
*                            FILE STATUS    IS   JYO-ST.
*プリンター
     SELECT     PRTF         ASSIGN    TO        GS-PRTF
                             DESTINATION        "PRT"
                             FORMAT              PRT-FORM
                             GROUP               PRT-GRP
                             PROCESSING          PRT-PROC
                             UNIT CONTROL        PRT-CTL
                             FILE      STATUS    PRT-ST.
*画面ファイル
     SELECT   DSPF           ASSIGN    TO        GS-DSPF
                             FORMAT              DSP-FMT
                             GROUP               DSP-GRP
                             PROCESSING          DSP-PRO
                             FUNCTION            DSP-FNC
                             STATUS              DSP-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*伝票データ
 FD  HKYOTU1
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     KYO       AS        PREFIX.
*取引先マスタ
*FD  HTOKMS
*    BLOCK       CONTAINS   8        RECORDS
*    LABEL       RECORD    IS        STANDARD.
*    COPY        HTOKMS    OF        XFDLIB
*    JOINING     TOK       AS        PREFIX.
*条件ファイル
*FD  HJYOKEN
*    BLOCK       CONTAINS  24        RECORDS
*    LABEL       RECORD    IS        STANDARD.
*    COPY        HJYOKEN   OF        XFDLIB
*    JOINING     JYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE24011  OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE24012  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*ファイルステイタス
 01  ST-AREA.
     03  KYO-ST                   PIC  X(02).
*    03  TOK-ST                   PIC  X(02).
*    03  JYO-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
     03  IN-DATA                  PIC  X(01).
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  DSP-FMT                  PIC X(08).
     03  DSP-PRO                  PIC X(02).
     03  DSP-GRP                  PIC X(08).
     03  DSP-FNC                  PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL            PIC X(04).
         05  DSP-STR-PG           PIC X(02).
     03  PRT-FORM                 PIC X(08).
     03  PRT-PROC                 PIC X(02).
     03  PRT-GRP                  PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL            PIC X(04).
         05  PRT-STR-PG           PIC X(02).
*フラグエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  9(01)     VALUE   ZERO.
     03  MAIN-END                 PIC  9(01)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
*カウンタ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-F111                  PIC  9(08)     VALUE   ZERO.
     03  WK-F15                   PIC  9(09)V99  VALUE   ZERO.
     03  WK-F172                  PIC  9(09)V99  VALUE   ZERO.
     03  WK-MAI                   PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  I                        PIC  9(01)     VALUE   ZERO.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  WK-DEN               PIC  9(09)     VALUE   ZERO.
     03  DENPYO-END               PIC  9(01)     VALUE   ZERO.
     03  OLD-F02                  PIC  9(09)     VALUE   ZERO.
     03  NEW-F02                  PIC  9(09)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG01                PIC  N(30)     VALUE
         NC"対象データありません".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『手書き伝票発行中』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了をこえています".
     03  MSG06                PIC  N(30)     VALUE
         NC"対象伝票がありません".
*
 01  KYO-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
*01  TOK-ERR                      PIC  N(11)     VALUE
*        NC"取引先マスタ異常！！".
*01  JYO-ERR                      PIC  N(11)     VALUE
*        NC"条件ファイル異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  PRT-ERR                      PIC  N(11)     VALUE
         NC"プリンター　異常！！".
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*伝票データ
 KYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HKYOTU1.
     DISPLAY  KYO-ERR          UPON    CONS.
     DISPLAY  KYO-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*取引先マスタ
*TOK-ERR                SECTION.
*    USE AFTER          EXCEPTION      PROCEDURE      HTOKMS.
*    DISPLAY  TOK-ERR          UPON    CONS.
*    DISPLAY  TOK-ST           UPON    CONS.
*    ACCEPT   IN-DATA          FROM    CONS.
*    MOVE     4000             TO      PROGRAM-STATUS.
*    STOP     RUN.
*条件ファイル
*JYO-ERR                SECTION.
*    USE AFTER          EXCEPTION      PROCEDURE      HJYOKEN.
*    DISPLAY  JYO-ERR          UPON    CONS.
*    DISPLAY  JYO-ST           UPON    CONS.
*    ACCEPT   IN-DATA          FROM    CONS.
*    MOVE     4000             TO      PROGRAM-STATUS.
*    STOP     RUN.
*プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    CONS.
     DISPLAY  PRT-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
*
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     MAIN-END  =  1.
     PERFORM     END-SEC.
*
     STOP        RUN.
*
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        I-O       DSPF
                 INPUT     HKYOTU1
*                          HTOKMS
*                          HJYOKEN
                 OUTPUT    PRTF.
*
     MOVE     SPACE              TO   FTE24012.
     MOVE     ZERO               TO   WK-DEN.
     MOVE     ZERO               TO   CNT-DENPYO.
*    MOVE     559                TO   R001.
*////PERFORM  DSP-WRT-SEC.
*
*****ACCEPT   IN-DATA  FROM CONS.
*
*帳票初期化*
     MOVE        SPACE     TO        FTE24011.
*伝票枚数カウント
     MOVE     ZERO         TO    WK-MAI.
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE           TO  KYO-REC.
     INITIALIZE  KYO-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     559          TO    KYO-F01.
     MOVE     ZERO         TO    KYO-F02.
     MOVE     ZERO         TO    KYO-F04
                                 KYO-F051
                                 KYO-F03.
*
*  2011/10/05,S  S.I/NAV
**     START    HKYOTU1       KEY  IS >=
**              KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
**          INVALID  KEY
**              DISPLAY NC"出力対象無し" UPON CONS
**              STOP  RUN
**     END-START.
     START    HKYOTU1       KEY  IS >=
              KYO-F01  KYO-F02  KYO-F04  KYO-F051
              KYO-F07  KYO-F112 KYO-F03
          INVALID  KEY
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*  2011/10/05,E  S.I/NAV
*
     PERFORM  DEN-CNT-SEC
              UNTIL         DENPYO-END = 9.
*
     IF       WK-MAI        =      ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                       伝票_カウント                    *
***********************************************************
 DEN-CNT-SEC               SECTION.
*
     MOVE     NEW-F02      TO   OLD-F02.
*
     READ     HKYOTU1
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*
     IF       KYO-F01      >    559
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*
     MOVE     KYO-F02  TO        NEW-F02.
*
     IF   OLD-F02      NOT =     NEW-F02
          ADD       1  TO        WK-MAI
     END-IF.
*
 DEN-CNT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-WRT-SEC.
 MAIN-010.
     MOVE         "KUBUN"    TO        DSP-GRP.
     MOVE        SPACE       TO        ERRMSG.
     PERFORM     DSP-RD-SEC.
*
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE    1         TO        MAIN-END
           GO                TO        MAIN-EXIT
       WHEN
        "E000"
           IF    KUBUN       NOT =     1 AND 2 AND 3
                 MOVE  MSG02       TO        ERRMSG
                 PERFORM                     DSP-WRT-SEC
                 GO                TO        MAIN-010
           END-IF
       WHEN
         OTHER
           MOVE    MSG04     TO        ERRMSG
           PERFORM                     DSP-WRT-SEC
           GO                TO        MAIN-010
     END-EVALUATE.
*
     MOVE        SPACE       TO        ERRMSG.
     PERFORM     DSP-WRT-SEC.
     EVALUATE      KUBUN
         WHEN      "1"
                   PERFORM   TEST-PRT-SEC
         WHEN      "2"
                   MOVE      ZERO      TO   I
                   MOVE      ZERO      TO   KAIDEN
                   MOVE      ALL "9"   TO   ENDDEN
                   MOVE      MSG03     TO   ERRMSG
                   PERFORM                  DSP-WRT-SEC
                   PERFORM   KYO-START-SEC
                   PERFORM   KYO-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                             PERFORM   DENP-WRT-SEC
                   END-IF

         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   IF        MAIN-END  =  1
                             GO          TO   MAIN-EXIT
                   ELSE
                             MOVE MSG03  TO   ERRMSG
                             PERFORM          DSP-WRT-SEC
                   END-IF
                   PERFORM   KYO-START-SEC
                   PERFORM   KYO-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                             PERFORM   DENP-WRT-SEC
                   END-IF
         WHEN      OTHER
                   MOVE      MSG02     TO   ERRMSG
     END-EVALUATE.
*
     CLOSE         PRTF.
     OPEN          OUTPUT    PRTF.
     MOVE          SPACE     TO  FTE24012.
     MOVE          ZERO      TO  WK-DEN.
     MOVE          ZERO      TO  END-FLG.
*
     PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       伝票データ　ＳＴＡＲＴ　         *
**********************************************************
 KYO-START-SEC             SECTION.
*
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE           TO  KYO-REC.
     INITIALIZE  KYO-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     559          TO    KYO-F01.
     MOVE     KAIDEN       TO    KYO-F02.
     MOVE     ZERO         TO    KYO-F04
                                 KYO-F051
                                 KYO-F03.
*
*  2011/10/05,S  S.I/NAV
**     START    HKYOTU1       KEY  IS >=
**              KYO-F01 KYO-F02 KYO-F04 KYO-F051 KYO-F03
**          INVALID  KEY
**              MOVE     1    TO    END-FLG
**              GO TO         KYO-START-EXIT
**     END-START.
     START    HKYOTU1       KEY  IS >=
              KYO-F01  KYO-F02  KYO-F04  KYO-F051
              KYO-F07  KYO-F112 KYO-F03
          INVALID  KEY
              MOVE     1    TO    END-FLG
              GO TO         KYO-START-EXIT
     END-START.
*  2011/10/05,E  S.I/NAV
*
*
 KYO-START-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
*****IF      (WK-DEN  NOT =   ZERO)
*****         PERFORM   TAIL-EDT-SEC
*****         PERFORM   DENP-WRT-SEC
*****END-IF.
*
     CLOSE    HKYOTU1   PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
     MOVE     "ﾃｶﾞｷ"              TO   HD01.
     MOVE     "ﾊﾝｽﾞﾏﾝ"            TO   HD02.
     MOVE     KYO-F02             TO   HD03.
     MOVE     KYO-F30             TO   HD04.
     MOVE     KYO-F07(4:2)        TO   HD05.
     MOVE     KYO-F12(1:2)        TO   HD06.
     MOVE     KYO-F132            TO   HD07.
     MOVE     KYO-F111            TO   WK-F111.
     MOVE     WK-F111(3:2)        TO   HD08.
     MOVE     WK-F111(5:2)        TO   HD09.
     MOVE     WK-F111(7:2)        TO   HD10.
     MOVE     KYO-F01             TO   HD11.
     MOVE   NC"_サカタのタネ"    TO   HD12.
*    MOVE     "(ｶﾌﾞ)ｻｶﾀﾉﾀﾈ"       TO   HD12.
*****MOVE     住所                TO   HD13.
*****MOVE     ＴＥＬ              TO   HD14.
*
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     ADD         1         TO        I.
*
     MOVE     KYO-F1421           TO   DM01(I).
     MOVE     KYO-F1422           TO   DM02(I).
     MOVE     KYO-F1411(1:7)      TO   DM03(I).
     MOVE     "("                 TO   DM12(I).
     MOVE     ")"                 TO   DM13(I).
     MOVE     KYO-F25(1:7)        TO   DM04(I).
     MOVE     KYO-F15             TO   WK-F15.
     MOVE     WK-F15(1:9)         TO   DM05(I).
     MOVE     WK-F15(10:1)        TO   DM06(I).
*原価計算
     MOVE     KYO-F172            TO   WK-F172.
     MOVE     WK-F172(1:9)        TO   DM07(I).
     MOVE     WK-F172(10:2)       TO   DM08(I).
     COMPUTE  W-GENKA  =  KYO-F15  *  KYO-F172.
     MOVE     W-GENKA             TO   DM09(I).
*売価計算
     MOVE     KYO-F173            TO   DM10(I).
     COMPUTE  W-BAIKA  =  KYO-F15  *  KYO-F173.
     MOVE     W-BAIKA             TO   DM11(I).
*原単価・売単価加算
     ADD      W-GENKA             TO   G-GENKA.
     ADD      W-BAIKA             TO   G-BAIKA.
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
     MOVE     G-GENKA             TO   TL01.
     MOVE     G-BAIKA             TO   TL02.
*原単価合計・売単価合計の初期化
     MOVE     ZERO                TO   G-GENKA.
     MOVE     ZERO                TO   G-BAIKA.
 TAIL-EDT-EXIT.
     EXIT.
**********************************************************
*                 データ編集書き出し                     *
**********************************************************
 DENP-WRT-SEC                  SECTION.
**************
*帳票書き出し*
**************
     MOVE       "FTE24011"
                           TO        PRT-FORM.
     MOVE       "ALLF"     TO        PRT-GRP.
     WRITE       FTE24011.
*帳票初期化*
     MOVE        SPACE     TO        FTE24011.
     MOVE        ZERO      TO        I.
 DENP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
*    MOVE     559       TO        R001.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE24012" TO        DSP-FMT.
     WRITE    FTE24012.
*
     MOVE     SPACE     TO        ERRMSG.
 DSP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-RD-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPF.
*
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
 KEY-IN-01.
     MOVE         "NE"       TO   DSP-PRO.
     MOVE         "KEY01"    TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE      1         TO   MAIN-END
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      MSG04     TO   ERRMSG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
****************
*エラーチェック*
****************
*
     IF  (KAIDEN  NOT  NUMERIC)
         MOVE    ZERO      TO        KAIDEN.
*
     IF  (ENDDEN  NOT  NUMERIC)
     OR  (ENDDEN = ZERO)
         MOVE  ALL "9"     TO        ENDDEN.
*出力区分チェック
     IF  (KUBUN  =  "1"  OR  "2"  OR  "3")
         MOVE   "M"        TO        EDIT-OPTION  OF    KUBUN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KUBUN
       ELSE
         MOVE   "R"        TO        EDIT-OPTION  OF    KUBUN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KUBUN
         MOVE    MSG02     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01.
*伝票_大小チェック
     IF  (KAIDEN > ENDDEN)
         MOVE   "R"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAIDEN
         MOVE    MSG05     TO        ERRMSG
         PERFORM DSP-WRT-SEC
         GO                TO        KEY-IN-01
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAIDEN
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDDEN
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAIDEN
     END-IF.
*項目表示
     PERFORM     DSP-WRT-SEC.
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 KYO-READ-SEC            SECTION.
*
 READ-000.
     READ     HKYOTU1   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   KYO-READ-EXIT
     END-READ.
*
*取引先コードが範囲外の時対象外
     IF       KYO-F01  >   559
              MOVE      1         TO   END-FLG
              GO                  TO   KYO-READ-EXIT
     END-IF.

*
     IF      (KYO-F02  <   KAIDEN)
              GO                  TO   READ-000
     END-IF.
     IF      (KYO-F02  >   ENDDEN)
              MOVE      1         TO   END-FLG
              GO                  TO   KYO-READ-EXIT
     END-IF.
*    格納伝票番号＝ＺＥＲＯの時
     IF      (WK-DEN  =   ZERO)
              PERFORM   HEAD-EDT-SEC
              MOVE      KYO-F02   TO   WK-DEN.
*    行_＝８０の時
     IF      (KYO-F03  =   80)
*             MOVE      KYO-F1421 TO   SHO1
*             MOVE      KYO-F1422 TO   SHO2
              GO                  TO   READ-000.
*    行_＝９０の時
     IF      (KYO-F03  =   90)
              GO                  TO   READ-000.
 READ-010.
*****DISPLAY "KYO-F02 = " KYO-F02 UPON  CONS.
*****DISPLAY "WK-DEN  = " WK-DEN  UPON  CONS.
     IF       KYO-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      KYO-F02   TO   WK-DEN
     END-IF.
*
     PERFORM  BODY-EDT-SEC.
 KYO-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

          MOVE   ALL "9"         TO   DM03(I) DM04(I) DM05(I)
                                      DM06(I) DM07(I) DM08(I)
                                      DM09(I) DM10(I) DM11(I)
          MOVE   ALL "*"         TO   DM01(I) DM02(I)
          MOVE   "("             TO   DM12(I)
          MOVE   ")"             TO   DM13(I)
     END-PERFORM.
*
     MOVE   ALL "9"              TO   HD03    HD05    HD06
                                      HD07    HD08    HD09
                                      HD10    HD11    TL01
                                      TL02.
*
     MOVE   ALL "*"              TO   HD01    HD02    HD04.
*
     MOVE   ALL NC"＊"           TO   HD12.
*伝票発行
     PERFORM  DENP-WRT-SEC.
*
     MOVE       1                TO   END-FLG.
 TEST-PRT-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE2401B.
******************************************************************

```
