***********************************************************
*   顧客名　　    ：   （株）サカタのタネ営業第二部       *
*   システム名    ：   流通ＢＭＳオンライン               *
*   サブシステム名：   ムサシ流通ＢＭＳ                   *
*   プログラム名　：   仕入伝票発行データ抽出：手書ムサシ *
*   プログラムID  ：   SSY4477I                           *
*   作成者        ：   NAV                                *
*   作成日        ：   2018/06/14   T.TAKAHASHI           *
*   更新日／更新者：                                      *
*   更新概要      ：                                      *
***********************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY4477I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/06/14.
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
     SELECT   WKDENAR        ASSIGN    TO        DA-01-VI-WKDENAR1
                             ORGANIZATION   IS   INDEXED
                             ACCESS    MODE IS   SEQUENTIAL
                             RECORD    KEY  IS   DEN-F013
                                                 DEN-F302
                                                 DEN-F304
                                                 DEN-F345
                                                 DEN-F402
                             FILE STATUS    IS   DEN-ST.
*画面
     SELECT   DSPF           ASSIGN    TO        GS-DSPF
                             FORMAT              DSP-FMT
                             GROUP               DSP-GRP
                             PROCESSING          DSP-PRO
                             FUNCTION            DSP-FNC
                             STATUS              DSP-ST.
*伝票発行ワーク
     SELECT   ARDENWK        ASSIGN    TO        DA-01-VS-ARDENWK
                             ACCESS    MODE IS   SEQUENTIAL
                             FILE STATUS    IS   DWK-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*伝票データ
 FD  WKDENAR.
     COPY        WKDENAR   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*伝票発行ワーク
 FD  ARDENWK.
     COPY        ARDENWK   OF        XFDLIB
     JOINING     DWK       AS        PREFIX.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY44771   OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*得意先ＣＤ
 01  WK-TOKCD                     PIC  9(08)  VALUE  116501.
*ファイルステイタス
 01  ST-AREA.
     03  DEN-ST                   PIC  X(02).
     03  DWK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
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
     03  INPUT-CNT                PIC  9(07)     VALUE   ZERO.
     03  OUTPUT-CNT               PIC  9(07)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE   ZERO.
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
*伝票番号変換
 01  WK-DENNO                     PIC  X(10).
 01  FILLER                       REDEFINES  WK-DENNO.
     02  WK-DENNO-H1              PIC  9(08).
     02  WK-DENNO-H2              PIC  X(02).
*
 01  DEN-ERR                      PIC  N(11)     VALUE
         NC"伝票データ　異常！！".
 01  DWK-ERR                      PIC  N(11)     VALUE
         NC"伝票発行ＷＫ異常！！".
 01  DSP-ERR                      PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG01                PIC  N(30)     VALUE
         NC"対象データがありません".
     03  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください".
     03  MSG03                PIC  N(30)     VALUE
         NC"『データ抽出中！！』".
     03  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG05                PIC  N(30)     VALUE
         NC"開始が終了をこえています".
     03  MSG06                PIC  N(30)     VALUE
         NC"対象伝票がありません".
     03  MSG07                PIC  N(30)     VALUE
         NC"テストプリントは行えません".
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*伝票データ
 DEN-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      WKDENAR.
     DISPLAY  DEN-ERR          UPON    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
     ACCEPT   IN-DATA          FROM    CONS.
     MOVE     4000             TO      PROGRAM-STATUS.
     STOP     RUN.
*伝票発行ワーク
 DWK-ERR                SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      ARDENWK.
     DISPLAY  DWK-ERR          UPON    CONS.
     DISPLAY  DWK-ST           UPON    CONS.
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
     OPEN        INPUT     WKDENAR.
     OPEN        OUTPUT    ARDENWK.
*
     MOVE     SPACE              TO   FSY44771.
     MOVE     ZERO               TO   INPUT-CNT  OUTPUT-CNT.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     WK-TOKCD     TO    R001.
*帳票初期化*
     MOVE        SPACE     TO    FSY44771.
*伝票枚数カウント
     MOVE     ZERO         TO    WK-MAI.
***2011.10.07 ST
     MOVE     SPACE        TO    DEN-REC.
     INITIALIZE                  DEN-REC.
***2011.10.07 EN
     MOVE     WK-TOKCD     TO    DEN-F013.
*スタート処理
     START    WKDENAR       KEY  IS >=
              DEN-F013  DEN-F302  DEN-F304  DEN-F345  DEN-F402
          INVALID  KEY
              DISPLAY NC"出力対象無し" UPON CONS
              MOVE 4000    TO      PROGRAM-STATUS
              STOP  RUN
     END-START.
*伝票ジャーナル読込み
     PERFORM  DEN-CNT-SEC
              UNTIL         DENPYO-END = 9.
*全枚数表示
     IF       WK-MAI        =      ZERO
              DISPLAY NC"出力対象無し" UPON CONS
              MOVE 4000    TO      PROGRAM-STATUS
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
     READ     WKDENAR
          AT END    MOVE   9    TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-READ.
*
     IF       DEN-F013   >    WK-TOKCD
          MOVE      9           TO    DENPYO-END
          GO TO                       DEN-CNT-EXIT
     END-IF.
*
     MOVE     DEN-F302     TO    WK-DENNO.
     MOVE     WK-DENNO-H1  TO    NEW-F02.
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
           IF    KUBUN       =     1
                 MOVE  MSG07       TO        ERRMSG
                 PERFORM                     DSP-WRT-SEC
                 GO                TO        MAIN-010
           ELSE
                 IF   KUBUN  = 2 OR 3
                      CONTINUE
                 ELSE
                      MOVE MSG02   TO        ERRMSG
                      PERFORM                DSP-WRT-SEC
                      GO           TO        MAIN-010
                 END-IF
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
*
     MOVE          SPACE     TO  FSY44771.
     MOVE          ZERO      TO  WK-DEN.
     MOVE          ZERO      TO  END-FLG.
     MOVE          1         TO  MAIN-END.
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
*
     MOVE     WK-TOKCD     TO    DEN-F013.
     MOVE     KAIDEN(2:8)  TO    DEN-F302(1:8).
     MOVE     SPACE        TO    DEN-F302(9:2).
*
     START    WKDENAR       KEY  IS >=
              DEN-F013  DEN-F302  DEN-F304  DEN-F345  DEN-F402
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
*
     CLOSE    WKDENAR   ARDENWK  DSPF.
 END-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 DATA-SET-SEC                  SECTION.
*
     MOVE        SPACE     TO        DWK-REC.
     INITIALIZE                      DWK-REC.
*
     MOVE        DEN-REC   TO        DWK-REC.
     WRITE       DWK-REC.
     ADD         1         TO        OUTPUT-CNT.
*
 DATA-SET-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRT-SEC          SECTION.
*画面表示
     MOVE     SPACE     TO        FORM-PARA.
     MOVE     WK-TOKCD  TO        R001.
     MOVE     WK-MAI    TO        DENMAI.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FSY44771" TO        DSP-FMT.
     WRITE    FSY44771.
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
 DEN-READ-SEC            SECTION.
*
 READ-000.
     READ     WKDENAR   AT        END
              MOVE      1         TO   END-FLG
              MOVE      MSG01     TO   ERRMSG
              GO                  TO   DEN-READ-EXIT
     END-READ.
*
*取引先コードが範囲外の時対象外
     IF       DEN-F013 >   WK-TOKCD
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.

*
     MOVE     DEN-F302            TO   WK-DENNO.
     IF      (WK-DENNO-H1  <   KAIDEN)
              GO                  TO   READ-000
     END-IF.
     IF      (WK-DENNO-H1  >   ENDDEN)
              MOVE      1         TO   END-FLG
              GO                  TO   DEN-READ-EXIT
     END-IF.
*
     PERFORM  DATA-SET-SEC.
*
 DEN-READ-EXIT.
     EXIT.
******************************************************************
*END PROGRAM  SSY4477I.
******************************************************************
