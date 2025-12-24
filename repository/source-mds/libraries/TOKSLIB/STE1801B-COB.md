# STE1801B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STE1801B.COB`

## ソースコード

```cobol
***********************************************************
*   顧客名　　    ：   （株）サカタのタネ殿
*   システム名    ：   販売管理システム
*   サブシステム名：   手書伝票発行　
*   プログラム名　：   ビーバー　手書伝票発行　　　
*   プログラムID  ：   STE1801B
*   作成者        ：   高橋　　
*   作成日        ：   1999.12.09
*   更新履歴      ：
*     2011/10/05 飯田/NAV 基幹サーバ統合
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE1801B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          96/04/22.
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
     SELECT   DENJNL1        ASSIGN    TO        DA-01-VI-JHTTEGL1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   SEQUENTIAL
                             RECORD    KEY  IS   DEN-F01
                                                 DEN-F02
                                                 DEN-F04
                                                 DEN-F051
                                       *> 2011/10/05,S  S.I/NAV
                                                 DEN-F07
                                                 DEN-F112
                                       *> 2011/10/05,E  S.I/NAV
                                                 DEN-F03
                             FILE STATUS    IS   DEN-ST.
*取引先マスタ
     SELECT   HTOKMS         ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TOK-F01
                             FILE STATUS    IS   TOK-ST.
*条件ファイル
     SELECT   HJYOKEN        ASSIGN    TO        DA-01-VI-JYOKEN1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   DYNAMIC
                             RECORD    KEY  IS   JYO-F01
                                                 JYO-F02
                             FILE STATUS    IS   JYO-ST.
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
 FD  DENJNL1
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE18011  OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE18012   OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*ファイルステイタス
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
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
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
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
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  TOK-ERR                      PIC  N(11)     VALUE
         NC"取引先マスタ異常！！".
 01  JYO-ERR                      PIC  N(11)     VALUE
         NC"条件ファイル異常！！".
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
 DEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DENJNL1.
     DISPLAY  DEN-ERR          UPON    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*取引先マスタ
 TOK-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTOKMS.
     DISPLAY  TOK-ERR          UPON    CONS.
     DISPLAY  TOK-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*条件ファイル
 JYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HJYOKEN.
     DISPLAY  JYO-ERR          UPON    CONS.
     DISPLAY  JYO-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
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
                 INPUT     DENJNL1
                           HTOKMS
                           HJYOKEN
                 OUTPUT    PRTF.
*
     MOVE     SPACE              TO   FTE18012.
     MOVE     ZERO               TO   WK-DEN.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     6002                TO   R001.
*////PERFORM  DSP-WRT-SEC.
*
*****ACCEPT   IN-DATA  FROM CONS.
*
*帳票初期化*
     MOVE        SPACE     TO        FTE18011.
*伝票枚数カウント
     MOVE     ZERO         TO    WK-MAI.
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE           TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     6002         TO    DEN-F01.
     MOVE     ZERO         TO    DEN-F02.
     MOVE     ZERO         TO    DEN-F04
                                 DEN-F051
                                 DEN-F03.
*
*  2011/10/05,S  S.I/NAV
**     START    DENJNL1       KEY  IS >=
**              DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F03
**          INVALID  KEY
**              DISPLAY NC"出力対象無し" UPON CONS
**              STOP  RUN
**     END-START.
     START    DENJNL1       KEY  IS >=
              DEN-F01  DEN-F02  DEN-F04  DEN-F051
              DEN-F07  DEN-F112 DEN-F03
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
*                       伝票Ｎｏカウント                  *
***********************************************************
 DEN-CNT-SEC               SECTION.
*
     MOVE     NEW-F02      TO   OLD-F02.
*
     READ     DENJNL1
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*
     IF       DEN-F01      >    6002
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*
     MOVE     DEN-F02  TO        NEW-F02.
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
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                             PERFORM   DENP-WRT-SEC
                   END-IF

         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
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
     MOVE          SPACE     TO  FTE18012.
     MOVE          ZERO      TO  WK-DEN.
     MOVE          ZERO      TO  END-FLG.
*
     PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       伝票データ　ＳＴＡＲＴ　         *
**********************************************************
 DEN-START-SEC             SECTION.
*
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE           TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     6002          TO    DEN-F01.
     MOVE     KAIDEN       TO    DEN-F02.
     MOVE     ZERO         TO    DEN-F04
                                 DEN-F051
                                 DEN-F03.
*
*  2011/10/05,S  S.I/NAV
**     START    DENJNL1       KEY  IS >=
**              DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F03
**          INVALID  KEY
**              MOVE     1    TO    END-FLG
**              GO TO         DEN-START-EXIT
**     END-START.
     START    DENJNL1       KEY  IS >=
              DEN-F01  DEN-F02  DEN-F04  DEN-F051
              DEN-F07  DEN-F112 DEN-F03
          INVALID  KEY
              MOVE     1    TO    END-FLG
              GO TO         DEN-START-EXIT
     END-START.
*  2011/10/05,E  S.I/NAV
*
*
 DEN-START-EXIT.
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
     CLOSE    HTOKMS   DENJNL1   HJYOKEN   PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
     MOVE        DEN-F112  TO        WK-DEN111.
     MOVE        WK-DEN111Y  TO      W004Y.
     MOVE        WK-DEN111M  TO      W004M.
     MOVE        WK-DEN111D  TO      W004D.
     MOVE        DEN-F112  TO        WK-DEN112.
     MOVE        WK-DEN112Y  TO      W005Y.
     MOVE        WK-DEN112M  TO      W005M.
     MOVE        WK-DEN112D  TO      W005D.
     MOVE        DEN-F07   TO        W009.
     MOVE        DEN-F12   TO        W010.
     MOVE        DEN-F132  TO        W011.
     MOVE        DEN-F02   TO        W012 FDEN.
     MOVE        DEN-F01   TO        W013.
*
     MOVE        DEN-F01   TO        TOK-F01.
     READ        HTOKMS
       INVALID
         MOVE    ALL "*"   TO        W001
       NOT INVALID
         MOVE    TOK-F04   TO        W001
     END-READ.
*
     MOVE        DEN-F30   TO        W008.
*
     MOVE        12        TO        JYO-F01.
     MOVE        SPACE     TO        JYO-F02.
     READ        HJYOKEN
       INVALID
         MOVE    ALL "*"   TO        W014
       NOT INVALID
         MOVE    JYO-F03   TO        W014
     END-READ.
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     ADD         1         TO        I.
*
     MOVE        DEN-F1421 TO        W15A(I).
     MOVE        DEN-F1422 TO        W15B(I).
     MOVE        DEN-F25(1:5)   TO   SYOCD1(I).
     MOVE        DEN-F25(6:8)   TO   W016(I).
*数量
     MOVE        DEN-F15   TO        WK-DEN15.
     MOVE        WK-DEN15A TO        W20A(I).
*
     IF   WK-DEN15B  NOT = ZERO
       MOVE      WK-DEN15B TO        W20B(I)
     END-IF.
     MOVE        DEN-F172  TO        WK-DEN172.
     MOVE        WK-DEN172A  TO      W23A(I).
     MOVE        WK-DEN172B  TO      W23B(I).
     MOVE        DEN-F173  TO        W025(I).
     COMPUTE     W-GENKA  =  DEN-F15  *  DEN-F172.
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-GENKA   TO        W024(I).
*****MOVE        W-BAIKA   TO        W026(I).
     ADD         W-GENKA   TO        G-GENKA.
     ADD         W-BAIKA   TO        G-BAIKA.
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
     MOVE        G-GENKA   TO        W027.
     MOVE        G-BAIKA   TO        W028.
     MOVE        ZERO      TO        G-GENKA.
     MOVE        ZERO      TO        G-BAIKA.
 TAIL-EDT-EXIT.
     EXIT.
**********************************************************
*                 データ編集書き出し               *
**********************************************************
 DENP-WRT-SEC                  SECTION.
**************
*帳票書き出し*
**************
     MOVE       "FTE18011" TO        PRT-FORM.
     MOVE       "ALLF"     TO        PRT-GRP.
     WRITE       FTE18011.
*帳票初期化*
     MOVE        SPACE     TO        FTE18011.
     MOVE        ZERO      TO        I.
 DENP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     6002       TO        R001.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE18012"  TO        DSP-FMT.
     WRITE    FTE18012.
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
*伝票Ｎｏ大小チェック
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
 DEN-READ-SEC            SECTION.
*
 READ-000.
     READ     DENJNL1   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   DEN-READ-EXIT
     END-READ.
*
*取引先コードが範囲外の時対象外
     IF       DEN-F01  >   6002
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.

*
     IF      (DEN-F02  <   KAIDEN)
              GO                  TO   READ-000
     END-IF.
     IF      (DEN-F02  >   ENDDEN)
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.
*
     IF      (WK-DEN  =   ZERO)
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN.
*
     IF      (DEN-F03  =   80)
              MOVE      DEN-F1421 TO   SHO1
              MOVE      DEN-F1422 TO   SHO2
              GO                  TO   READ-000.
     IF      (DEN-F03  =   90)
              GO                  TO   READ-000.
 READ-010.
     IF       DEN-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN
     END-IF.
*
     PERFORM  BODY-EDT-SEC.
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

**********MOVE   ALL "9"         TO   W20B(I) W23A(I) W23B(I)
          MOVE   ALL "9"         TO           W23A(I) W23B(I)
**************************************W024(I) W025(I) W026(I)
                                      W024(I) W025(I)
                                      W20A(I)
          MOVE   ALL "*"         TO   W15A(I) W15B(I) W016(I)
     END-PERFORM.
*
     MOVE   ALL "9"              TO   W004Y   W004M   W004D
                                      W005Y   W005M   W005D
                                      W011    W013    W009
                                      W012    W027    W028.
*
     MOVE   ALL "*"              TO   W008    W010    W001
                                      SHO1    SHO2.
*
     MOVE   ALL NC"＊"           TO   W014.
*伝票発行
     PERFORM  DENP-WRT-SEC.
*
     MOVE       1                TO   END-FLG.
 TEST-PRT-EXIT.
     EXIT.
******************************************************************
*END PROGRAM  STE1801B.
******************************************************************

```
