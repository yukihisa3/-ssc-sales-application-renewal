***********************************************************
*   顧客名　　    ：   _サカタのタネ殿                   *
*   システム名    ：   販売管理システム                   *
*   サブシステム名：   手書伝票発行　                     *
*   プログラム名　：   ジョイ手書伝票発行　　　　         *
*   プログラムID  ：   STE8801B                           *
*   作成者        ：   高橋　　                           *
*   作成日        ：   2012.02.27                         *
*   更新日        ：                                      *
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            STE8801B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/02/27.
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
                                                 DEN-F07
                                                 DEN-F112
                                                 DEN-F03
                             FILE STATUS    IS   DEN-ST.
*取引先マスタ
     SELECT   HTOKMS         ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TOK-F01
                             FILE STATUS    IS   TOK-ST.
*店舗マスタ
     SELECT   HTENMS         ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   RANDOM
                             RECORD    KEY  IS   TEN-F52  TEN-F011
                             FILE STATUS    IS   TEN-ST.
*画面
     SELECT   DSPF           ASSIGN    TO        GS-DSPF
                             FORMAT              DSP-FMT
                             GROUP               DSP-GRP
                             PROCESSING          DSP-PRO
                             FUNCTION            DSP-FNC
                             STATUS              DSP-ST.
*プリンター
     SELECT   PRTF           ASSIGN    TO        LP-04-PRTF.
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
*店舗マスタ
 FD  HTENMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*プリンター
 FD  PRTF                            LINAGE   IS   30    LINES.
 01  PRT-REC                         PIC X(200).
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FTE77011   OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*ファイルステイタス
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
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
***  ﾃﾞﾝﾋﾟｮｳﾊﾞﾝｺﾞｳﾍﾝｼｭｳ
 01  WK-DENPYO.
     03  WK-DENPYO-1              PIC  X(02)     VALUE   SPACE.
     03  WK-DENPYO-2              PIC  X(09)     VALUE   SPACE.
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
     03  WK-BIKO1                 PIC  X(15)     VALUE   SPACE.
     03  WK-BIKO2                 PIC  X(15)     VALUE   SPACE.
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
 01  TEN-ERR                      PIC  N(11)     VALUE
         NC"店舗マスタ異常！！　".
 01  JYO-ERR                      PIC  N(11)     VALUE
         NC"条件ファイル異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  PRT-ERR                      PIC  N(11)     VALUE
         NC"プリンター　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(12)     VALUE   SPACE.
     03  HD01-1                   PIC  X(30).
*
 01  HEAD-02.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD02-1                   PIC  X(20).
     03  FILLER                   PIC  X(45)     VALUE   SPACE.
     03  HD02-2                   PIC  X(20).
*
 01  HEAD-03.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-1                   PIC  X(20)     VALUE   SPACE.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-2                   PIC  X(12).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-3                   PIC  X(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-4                   PIC  X(02).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-5                   PIC  X(11).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-6                   PIC  X(06).
     03  FILLER                   PIC  X(26)     VALUE   SPACE.
     03  HD03-7                   PIC  9(06).
     03  HD03-8                   PIC  9(06).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-9                   PIC  X.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
*
*    ﾒｲｻｲ
 01  BODY-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD1-1                    PIC  X(25).
*
 01  BODY-02.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD2-1                    PIC  X(25).
     03  BD2-2                    PIC  X(13).
     03  BD2-3                    PIC  ZZZZZZZ.
     03  BD2-4                    PIC  ZZZZZ.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD2-51                   PIC  ZZZZ9.
     03  BD2-52                   PIC  Z.
     03  FILLER                   PIC  X(13)     VALUE   SPACE.
     03  BD2-61                   PIC  ZZZZZ9.
     03  BD2-62                   PIC  ZZ.
     03  BD2-7                    PIC  ZZZZZZZZ9.
     03  BD2-8                    PIC  ZZZZZZ.
     03  BD2-9                    PIC  ZZZZZZZZZ.
*
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  FILLER                   PIC  X(79)     VALUE   SPACE.
     03  TL1-1                    PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  TL1-2                    PIC  ZZZZZZZZZ.
*    ﾋﾞｺｳ
 01  BIKO-01.
     03  FILLER                   PIC  X(35)     VALUE   SPACE.
     03  BIK-01                   PIC  X(15).
 01  BIKO-02.
     03  FILLER                   PIC  X(35)     VALUE   SPACE.
     03  BIK-02                   PIC  X(15).
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
*店舗マスタ
 TEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HTENMS.
     DISPLAY  TEN-ERR          UPON    CONS.
     DISPLAY  TEN-ST           UPON    CONS.
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
     OPEN        I-O       DSPF.
     OPEN        INPUT     DENJNL1
                           HTOKMS
                           HTENMS.
     OPEN        OUTPUT    PRTF.
*
     MOVE     SPACE              TO   FTE77011.
     MOVE     ZERO               TO   WK-DEN  L-CNT.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     304021       TO   R001.
*帳票初期化*
     MOVE        SPACE     TO   FTE77011.
*伝票枚数カウント
     MOVE     ZERO         TO    WK-MAI.
     MOVE     SPACE        TO    DEN-REC.
     INITIALIZE                  DEN-REC.
     MOVE     304021       TO    DEN-F01.
     MOVE     ZERO         TO    DEN-F02.
     MOVE     ZERO         TO    DEN-F04
                                 DEN-F051
                                 DEN-F03.
*スタート処理
     START    DENJNL1       KEY  IS >=
              DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F07 DEN-F112
              DEN-F03
          INVALID  KEY
              DISPLAY NC"出力対象無し" UPON CONS
              STOP  RUN
     END-START.
*伝票ジャーナル読込み
     PERFORM  DEN-CNT-SEC
              UNTIL         DENPYO-END = 9.
*全枚数表示
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
     READ     DENJNL1
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*
     IF       DEN-F01      >    304021
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
                   END-IF

         WHEN      "3"
                   MOVE      ZERO      TO   I
                   PERFORM   KEY-IN-SEC
                   PERFORM   DEN-START-SEC
                   PERFORM   DEN-READ-SEC
                             UNTIL     END-FLG   =   1
                   IF       (WK-DEN  NOT =   ZERO)
                             PERFORM   TAIL-EDT-SEC
                   END-IF
         WHEN      OTHER
                   MOVE      MSG02     TO   ERRMSG
     END-EVALUATE.
*
     CLOSE         PRTF.
     OPEN          OUTPUT    PRTF.
     MOVE          SPACE     TO  FTE77011.
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
     MOVE     SPACE        TO    DEN-REC.
     INITIALIZE                  DEN-REC.
     MOVE     304021       TO    DEN-F01.
     MOVE     KAIDEN       TO    DEN-F02.
     MOVE     ZERO         TO    DEN-F04
                                 DEN-F051
                                 DEN-F03.
*
     START    DENJNL1       KEY  IS >=
              DEN-F01 DEN-F02 DEN-F04 DEN-F051 DEN-F07 DEN-F112
              DEN-F03
          INVALID  KEY
              MOVE     1    TO    END-FLG
              GO TO         DEN-START-EXIT
     END-START.
*
*
 DEN-START-EXIT.
     EXIT.
*********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE    HTOKMS   HTENMS   DENJNL1   PRTF.
*
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 HEAD-EDT-SEC                 SECTION.
*
     MOVE        SPACE     TO        HD01-1.
*発注日
     MOVE        DEN-F111  TO        HD03-7.
*納品日
     MOVE        DEN-F112  TO        HD03-8.
*店舗コード
     MOVE        DEN-F07   TO        HD03-2.
*分類コード
     MOVE        DEN-F12   TO        HD03-3.
*伝票区分
     MOVE        DEN-F132  TO        HD03-4.
*伝票番号
     MOVE        SPACE     TO        WK-DENPYO-1.
     MOVE        DEN-F02   TO        WK-DENPYO-2.
     MOVE        SPACE     TO        WK-DENPYO-2(1:2).
     MOVE        WK-DENPYO TO        HD03-5.
*取引先コード
*****MOVE        DEN-F01(3:6)  TO        HD03-6.
     MOVE        "030402"      TO        HD03-6.
*社名
     MOVE        DEN-F01   TO        TOK-F01.
     READ  HTOKMS  INVALID
           MOVE   ALL "*"  TO        HD02-1
           NOT INVALID
           MOVE   TOK-F04  TO        HD02-1
     END-READ.
*取引先名
     MOVE   "KK. ｻｶﾀﾉﾀﾈ"   TO        HD02-2.
*店舗名（カナ）
     MOVE        DEN-F01   TO        TEN-F52.
     MOVE        DEN-F07   TO        TEN-F011.
     READ  HTENMS  INVALID
           MOVE   ALL "*"  TO        HD03-1
           NOT INVALID
           MOVE   TEN-F04  TO        HD03-1
     END-READ.
*ヘッダ印字
     WRITE  PRT-REC  FROM  HEAD-01  AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-02  AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-03  AFTER  1.
     MOVE   SPACE          TO       PRT-REC.
     WRITE  PRT-REC  AFTER 1.
*行カウント
     ADD         7         TO        L-CNT.
*
 HEAD-EDT-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 BODY-EDT-SEC                  SECTION.
*商品名１
     MOVE        DEN-F1421 TO        BD1-1.
*商品名２
     MOVE        DEN-F1422 TO        BD2-1.
*商品コード
*****MOVE        DEN-F1411 TO        BD2-2.
     MOVE        DEN-F25   TO        BD2-2.
*数量
     MOVE        DEN-F15   TO        WK-DEN15.
     MOVE        WK-DEN15A TO        BD2-51.
*少数点編集
     IF   WK-DEN15B  NOT = ZERO
       MOVE      WK-DEN15B TO        BD2-52
     END-IF.
*原価単価（編集）
     MOVE        DEN-F172  TO        WK-DEN172.
     MOVE        WK-DEN172A  TO      BD2-61.
     MOVE        WK-DEN172B  TO      BD2-62.
*売価単価（編集）
     MOVE        DEN-F173  TO        BD2-8.
*原価金額計算
     COMPUTE     W-GENKA  =  DEN-F15  *  DEN-F172.
     MOVE        W-GENKA   TO        BD2-7.
*売価金額計算
     COMPUTE     W-BAIKA  =  DEN-F15  *  DEN-F173.
     MOVE        W-BAIKA   TO        BD2-9.
*合計エリア加算
     ADD         W-GENKA   TO        G-GENKA.
     ADD         W-BAIKA   TO        G-BAIKA.
*
     WRITE    PRT-REC    FROM    BODY-01    AFTER    1.
     WRITE    PRT-REC    FROM    BODY-02    AFTER    1.
     INITIALIZE          BODY-01   BODY-02.
     ADD         2         TO        L-CNT.
*
 BODY-EDT-EXIT.
     EXIT.
**********************************************************
*                 合計データ編集書き出し                 *
**********************************************************
 TAIL-EDT-SEC                  SECTION.
*原価金額合計
     MOVE        G-GENKA   TO        TL1-1.
*売価金額合計
     MOVE        G-BAIKA   TO        TL1-2.
*合計行印字調整
     COMPUTE     CNT-AFTER   =   27   -   L-CNT.
*    備考欄の転送
     MOVE   WK-BIKO1             TO   BIK-01.
     MOVE   WK-BIKO2             TO   BIK-02.
*合計行出力
     WRITE  PRT-REC  FROM  TAIL-01  AFTER  CNT-AFTER.
     WRITE  PRT-REC  FROM  BIKO-01  AFTER  2.
     WRITE  PRT-REC  FROM  BIKO-02  AFTER  1.
     MOVE        SPACE     TO        PRT-REC.
     WRITE       PRT-REC   AFTER     PAGE.
*ワーク合計計算エリア
     MOVE        ZERO      TO        G-GENKA.
     MOVE        ZERO      TO        G-BAIKA.
     MOVE        ZERO      TO        L-CNT.
     MOVE        SPACE     TO        WK-BIKO1  WK-BIKO2.
*
 TAIL-EDT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*画面表示
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     304021     TO        R001.
     MOVE     WK-MAI     TO        DENMAI.
     MOVE    "SCREEN"    TO        DSP-GRP.
     MOVE    "FTE77011"  TO        DSP-FMT.
     WRITE    FTE77011.
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
*
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
     IF       DEN-F01  >   304021
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
              MOVE     DEN-F1421  TO  WK-BIKO1
              MOVE     DEN-F1422  TO  WK-BIKO2
              GO                  TO   READ-000.
     IF      (DEN-F03  =   90)
              GO                  TO   READ-000.
 READ-010.
     IF       DEN-F02   NOT =     WK-DEN
              PERFORM   TAIL-EDT-SEC
              PERFORM   HEAD-EDT-SEC
              MOVE      DEN-F02   TO   WK-DEN
     END-IF.
     PERFORM  BODY-EDT-SEC.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*             テストプリント                        2.6        *
****************************************************************
 TEST-PRT-SEC           SECTION.
*テスト印字文字セット
     MOVE   ALL "9"              TO   HD03-5  HD03-7
                                      HD03-8  HD03-9.
     MOVE   ALL "9"              TO   BD2-3   BD2-4   BD2-7
                                      BD2-51  BD2-61
                                      BD2-52  BD2-62
                                      BD2-8   BD2-9.
     MOVE   ALL "9"              TO   TL1-1   TL1-2.
     MOVE   ALL "*"              TO   BD1-1   BD2-2   BD2-1
                                      HD01-1  HD02-1   HD02-2
                                      HD03-1  HD03-2   HD03-3
                                      HD03-4  HD03-6
                                      WK-BIKO1 WK-BIKO2.
*ヘッダ印字
     WRITE  PRT-REC  FROM  HEAD-01  AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-02  AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-03  AFTER  1.
     MOVE   SPACE          TO       PRT-REC.
     WRITE  PRT-REC  AFTER 1.
*明細印字
     PERFORM VARYING L-CNT FROM 1 BY 1 UNTIL L-CNT > 9
             WRITE  PRT-REC  FROM  BODY-01  AFTER  1
             WRITE  PRT-REC  FROM  BODY-02  AFTER  1
     END-PERFORM.
*テイル印字
     WRITE  PRT-REC  FROM  TAIL-01  AFTER  2.
     WRITE  PRT-REC  FROM  BIKO-01  AFTER  2.
     WRITE  PRT-REC  FROM  BIKO-02  AFTER  1.
*
     INITIALIZE      HEAD-01  HEAD-02  HEAD-03  BODY-01
                     BODY-02  TAIL-01.
     MOVE   ZERO           TO       L-CNT.
     MOVE   SPACE          TO       WK-BIKO1  WK-BIKO2.
*
 TEST-PRT-EXIT.
     EXIT.
******************************************************************
*END PROGRAM  STE8801B.
******************************************************************
