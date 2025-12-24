****************************************************************
*   SYSTEM ･･･ドイト　　オンライン　システム
*   PG-NAME･･･手書き伝票行
*   PG-ID  ･･･S T E 1 4 0 1 B
*   更新履歴：
*     2011/10/05 飯田/NAV 基幹サーバ統合
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE1401B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          92/12/07.
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
     SELECT   HDENJNL        ASSIGN    TO        DA-01-VI-JHTTEGL1
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
*店舗マスタ
     SELECT   HTENMS         ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TEN-F52
                                                 TEN-F011
                             FILE STATUS    IS   TEN-ST.
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
 FD  HDENJNL
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*店舗マスタ
 FD  HTENMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE14011    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE14012     OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*** ｽﾃｰﾀｽ
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  DEN-ST1                  PIC  X(04).
     03  TEN-ST                   PIC  X(02).
     03  TEN-ST1                  PIC  X(04).
     03  JYO-ST                   PIC  X(02).
     03  JYO-ST1                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
     03  DSP-ST1                  PIC  X(04).
     03  PRT-ST                   PIC  X(02).
     03  PRT-ST1                  PIC  X(04).
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
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  9(01)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
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
     03  W-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
 01  WK-KIKAKU                    PIC  X(03).
 01  WK-KIKAKU-R   REDEFINES      WK-KIKAKU.
     03  WK-KIKAKU-H              PIC  9(03).
*数量
 01  WK-DEN15                     PIC S9(09)V99.
 01  WK-DEN15R     REDEFINES      WK-DEN15.
     03  WK-DEN15A                PIC S9(09).
     03  WK-DEN15B                PIC  9(02).
*原価単価
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
     03  IX                       PIC  9(02)     VALUE   ZERO.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
 01  WK-NOUHIN                    PIC  9(06)     VALUE   ZERO.
 01  WK-HNOUHIN.
     03  WK-HNOUHIN1              PIC  Z9.
     03  WK-HNOUHIN1              PIC  Z9.
     03  WK-HNOUHIN3              PIC  Z9.
 01  WK-TORIHIKI                  PIC  9(05)     VALUE   ZERO.
 01  WK-F02                       PIC  9(06)     VALUE   ZERO.
 01  WK-W014A                     PIC  9(02)     VALUE   ZERO.
 01  WK-DENNO                     PIC  9(09)     VALUE   ZERO.
*
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  WK-DEN                   PIC  9(09)     VALUE   ZERO.
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
         NC"開始が終了より大きいです".
*
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  TEN-ERR                      PIC  N(11)     VALUE
         NC"店舗マスタ　異常！！".
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
     USE AFTER          EXCEPTION      PROCEDURE      HDENJNL.
     DISPLAY  DEN-ERR          UPON    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
*****MOVE     255              TO      PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
 TEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
     DISPLAY  TEN-ERR          UPON    CONS.
     DISPLAY  TEN-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
*****MOVE     255              TO      PROGRAM-STATUS.
     STOP     RUN.
*条件ファイル
 JYO-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HJYOKEN.
     DISPLAY  JYO-ERR          UPON    CONS.
     DISPLAY  JYO-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
*****MOVE     255              TO      PROGRAM-STATUS.
     STOP     RUN.
*プリンター
 PRT-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTF.
     DISPLAY  PRT-ERR          UPON    CONS.
     DISPLAY  PRT-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
*****MOVE     255              TO      PROGRAM-STATUS.
     STOP     RUN.
*画面ファイル
 DSP-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPF.
     DISPLAY  DSP-ERR          UPON    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
*****MOVE     255              TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 PROC-SEC                  SECTION.
     OPEN        I-O       DSPF.
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = 1 OR 9.
     PERFORM     END-SEC.
     IF  (END-FLG = 1)
         MOVE    ZERO     TO         END-FLG
         GO               TO         PROC-010.
     CLOSE       DSPF.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     HDENJNL
                           HTENMS
                           HJYOKEN
                 OUTPUT    PRTF.
*
     MOVE     ZERO               TO   WK-DEN WK-MAI WK-DENNO.
     MOVE     ZERO               TO   CNT-DENPYO.
*伝票ファイルスタート
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     10545              TO   DEN-F01.
     MOVE     ZERO               TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F04.
     MOVE     40                 TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
*  2011/10/05,S  S.I/NAV
**     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
**                                      DEN-F051 DEN-F03
**              INVALID
**              DISPLAY NC"出力対象無し" UPON CONS
**              STOP  RUN
**     END-START.
     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
                                      DEN-F051 DEN-F07  DEN-F112
                                      DEN-F03
              INVALID
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*  2011/10/05,E  S.I/NAV
 INIT001.
*伝票総枚数読込み
     READ     HDENJNL  AT  END
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-READ.
*取引先がブレイクの場合、読み飛ばし
     IF       DEN-F01   >  10545
         IF   WK-MAI  =  ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
         ELSE
              GO                 TO   INIT002
         END-IF
     END-IF.
*伝票行が８０以上の場合、読み飛ばし
     IF       DEN-F03  >=  80
              GO                 TO   INIT001
     END-IF.
*伝票ブレイクチェック
     IF       DEN-F02  NOT =  WK-DENNO
              ADD      1         TO   WK-MAI
              MOVE     DEN-F02   TO   WK-DENNO
     END-IF.
*
     GO                          TO   INIT001.
*
*****MOVE     DEN-F98            TO   WK-MAI.
 INIT002.
     CLOSE    HDENJNL.
*
     OPEN     INPUT     HDENJNL.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FTE14011.
     MOVE        SPACE     TO        FTE14012.
 INIT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-WRT-SEC.
 MAIN-010.
     MOVE         "KUBUN"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE    9         TO        END-FLG
           GO                TO        MAIN-EXIT
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
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
         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
                             UNTIL     END-FLG   =   1
         WHEN      OTHER
                   MOVE      MSG02     TO   ERRMSG
     END-EVALUATE.
     PERFORM       DSP-WRT-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     IF      (WK-DEN  NOT =   ZERO)
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
     END-IF.
     CLOSE       HTENMS    HDENJNL  HJYOKEN   PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
*特売表示
*****DISPLAY "DEN-F01  = " DEN-F01  UPON CONS.
*****DISPLAY "DEN-F131 = " DEN-F131 UPON CONS.
     IF   (DEN-F131  =  "02")
         MOVE    "ﾄｸﾊﾞｲ("    TO      W011A
         MOVE   DEN-F22(1:3) TO      WK-KIKAKU
         MOVE   WK-KIKAKU-H  TO      W011B
         MOVE    ")"         TO      W011C
     ELSE
         MOVE    SPACE       TO      W011A
         MOVE    ZERO        TO      W011B
         MOVE    SPACE       TO      W011C
     END-IF.
*納品日
*****MOVE        DEN-F112    TO      W017.
     MOVE        DEN-F111    TO      WK-NOUHIN.
     MOVE        WK-NOUHIN   TO      WK-HNOUHIN.
     MOVE        WK-HNOUHIN  TO      W017.
*
     MOVE        "02"        TO      W016.

*伝票Ｎｏ
     MOVE        DEN-F02     TO      WK-F02.
     MOVE        WK-F02      TO      W012.
*
*商品区分
     MOVE        DEN-F131  TO        W015.
*店舗コード，分類コード
*****MOVE        DEN-F07   TO        W014A.
     MOVE        DEN-F07   TO        WK-W014A.
     MOVE        WK-W014A  TO        W014A.
     MOVE        DEN-F12   TO        W014B.
*店舗名
     MOVE        DEN-F01   TO        TEN-F52.
     MOVE        DEN-F07   TO        TEN-F011.
     READ        HTENMS
       INVALID
         MOVE    ALL "*"   TO        W013
       NOT INVALID
         MOVE    TEN-F04   TO        W013
     END-READ.
*取引先コード，取引先名
*****MOVE        DEN-F01   TO        W018.
     MOVE        DEN-F01   TO        WK-TORIHIKI.
     MOVE        WK-TORIHIKI  TO     W018.
*注文Ｎｏ
     MOVE        DEN-F10   TO        TYUBAN.
*
     MOVE        12        TO        JYO-F01.
     MOVE        SPACE     TO        JYO-F02.
     READ        HJYOKEN
       INVALID
         MOVE    ALL "*"   TO        W019
       NOT INVALID
         MOVE    JYO-F03   TO        W019
     END-READ.
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
     IF          DEN-F03   <         7
                 ADD       1         TO        IX
     END-IF.
*商品コード，商品名
     MOVE        DEN-F1421 TO        W22A(IX).
     MOVE        DEN-F1422 TO        W22B(IX).
     MOVE        DEN-F25   TO        W023(IX).
*数量
     MOVE        DEN-F15   TO        WK-DEN15.
     MOVE        WK-DEN15A TO        W24A(IX).
     MOVE        WK-DEN15B TO        W24B(IX).
*原単価，売単価
     MOVE        DEN-F172  TO        WK-DEN172.
     MOVE        WK-DEN172A  TO      W25A(IX).
     MOVE        WK-DEN172B  TO      W25B(IX).
     MOVE        DEN-F173  TO        W027(IX).
*原価金額，売価金額
     COMPUTE     W-GENKA  =  DEN-F15  *  DEN-F172.
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-GENKA   TO        W026(IX).
     MOVE        W-BAIKA   TO        W028(IX).
     ADD         W-GENKA   TO        G-GENKA.
     ADD         W-BAIKA   TO        G-BAIKA.
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
     MOVE        G-GENKA   TO        W031.
     MOVE        G-BAIKA   TO        W032.
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
     MOVE       "FTE14011"   TO        PRT-FORM.
     MOVE       "ALLF"     TO        PRT-GRP.
     WRITE       FTE14011.
*帳票初期化*
     MOVE        SPACE     TO        FTE14011.
     MOVE        ZERO      TO        I.
 DENP-WRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FTE14012"    TO        DSP-FMT.
     WRITE    FTE14012.
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
                   MOVE      9         TO   END-FLG
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
     READ     HDENJNL   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   DEN-READ-EXIT
     END-READ.
*
     IF       DEN-F01  >   10545
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.
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
*94.03.09  START

              MOVE      ZERO      TO   IX
              MOVE      DEN-F02   TO   WK-DEN.
 READ-010.
     IF       DEN-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   DENP-WRT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      ZERO      TO   IX
              MOVE      DEN-F02   TO   WK-DEN
     END-IF.
*
*
     IF       DEN-F03   <    80
              PERFORM        BODY-EDT-SEC

*
     ELSE
        IF    DEN-F03   =    80
              MOVE           DEN-F1421  TO  BK001
              MOVE           DEN-F1422  TO  BK002
        END-IF
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*
     PERFORM   VARYING   I   FROM  1  BY  1   UNTIL   I  >  6

          MOVE   ALL "9"         TO   W24A(I) W24B(I) W25A(I)
                                      W25B(I) W026(I) W027(I)
                                      W028(I)
          MOVE   ALL "*"         TO   W22A(I) W22B(I) W023(I)

     END-PERFORM.
*
     MOVE   ALL "9"              TO   W016    W017    W018
                                      W014A   W014B   W015
                                      W031    W032.
*
     MOVE   ALL "*"              TO   W013 BK001 BK002 TYUBAN.
*
     MOVE   ALL NC"＊"           TO   W019.
*伝票発行
     PERFORM  DENP-WRT-SEC.
*
     MOVE       1                TO   END-FLG.
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             手書売上伝票ファイルスタート          2.6        *
****************************************************************
 DEN-START-SEC          SECTION.
*
*  2011/10/05,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
*  2011/10/05,E  S.I/NAV
     MOVE     10545              TO   DEN-F01.
     MOVE     KAIDEN             TO   DEN-F02.
     MOVE     ZERO               TO   DEN-F04.
     MOVE     40                 TO   DEN-F051.
     MOVE     ZERO               TO   DEN-F03.
**     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
**                                      DEN-F051 DEN-F03
**              INVALID
**              DISPLAY  "ﾃﾞﾝﾋﾟｮｳ ﾃﾞｰﾀ  ﾅｼ!!"  UPON  CONS
**              ACCEPT   IN-DATA               FROM  CONS
**              STOP  RUN
**     END-START.
     START    HDENJNL   KEY  IS  >=   DEN-F01  DEN-F02  DEN-F04
                                      DEN-F051 DEN-F07  DEN-F112
                                      DEN-F03
              INVALID
              DISPLAY  "ﾃﾞﾝﾋﾟｮｳ ﾃﾞｰﾀ  ﾅｼ!!"  UPON  CONS
              ACCEPT   IN-DATA               FROM  CONS
              STOP  RUN
     END-START.





*
 DEN-START-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  STE1401B.
******************************************************************
