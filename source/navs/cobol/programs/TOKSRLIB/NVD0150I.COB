****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定表発行指示　　　　　　　　*
*    作成日／作成者　　　：　2020/02/20   ASS.II               *
*    処理内容　　　　　　：　入荷予定表出力条件を入力して　　　*
*    　　　　　　　　　　　　対象データを抽出し、入荷予定　　　*
*                            リストWKを作成する。　　　　　　
*    更新日／更新者　　　：　2022/07/20   INOUE                *
*    更新内容　　　　　　：　指示項目に「出力区分」追加　　　　*
*    　　　　　　　　　　　　ＳＵＢ商品名称マスタ追加　　　　　*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         NVD0150I.
 AUTHOR.             NAV.
 DATE-WRITTEN.       20.02.20.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*画面Ｆ
     SELECT   DSPF           ASSIGN               TO  GS-DSPF
                             ORGANIZATION         IS  SEQUENTIAL
                             ACCESS MODE          IS  SEQUENTIAL
                             SYMBOLIC DESTINATION IS "DSP"
                             PROCESSING MODE      IS  DSP-PRO
                             GROUP                IS  DSP-GRP
                             FORMAT               IS  DSP-FMT
                             SELECTED FUNCTION    IS  DSP-FNC
                             FILE STATUS          IS  DSP-STA.

*発注Ｆ（ヘッダ）
     SELECT   HACHEDF        ASSIGN        TO  DA-01-VI-HACHEDL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  HED-F17
                                               HED-F02
                             FILE STATUS   IS  HED-STA.

*発注Ｆ(明細）
     SELECT   HACMEIF        ASSIGN        TO  DA-01-VI-HACMEIL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  BDY-F02
                                               BDY-F03
                             FILE STATUS   IS  BDY-STA.

*条件Ｆ
     SELECT   JYOKEN1        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  DA-01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*20220720
* SUB商品名称マスタ
     SELECT   SUBMEIL1       ASSIGN  TO   DA-01-VI-SUBMEIL1
                             ORGANIZATION         IS  INDEXED
                             ACCESS  MODE         IS  RANDOM
                             RECORD  KEY          IS  MEI-F011
                                                      MEI-F0121
                                                      MEI-F0122
                                                      MEI-F0123
                             FILE STATUS          IS  MEI-STA.
*抽出Ｆ
     SELECT   NYYOTEIF       ASSIGN        TO  DA-01-S-NYYOTEIF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  NYY-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FVD01501  OF  XMDLIB
     JOINING  DSP      AS  PREFIX.
*発注Ｆ（ヘッダ）
 FD  HACHEDF
                       LABEL     RECORD    IS   STANDARD.
     COPY     HACHEDF  OF  XFDLIB
     JOINING  HED      AS  PREFIX.
*発注Ｆ（明細）
 FD  HACMEIF
                       LABEL     RECORD    IS   STANDARD.
     COPY     HACMEIF  OF  XFDLIB
     JOINING  BDY      AS  PREFIX.
*条件Ｆ
 FD  JYOKEN1
                       LABEL     RECORD    IS   STANDARD.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1
                       LABEL     RECORD    IS   STANDARD.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*20220720
*ＳＵＢ商品名称マスタ
 FD  SUBMEIL1
                       LABEL     RECORD    IS   STANDARD.
     COPY     SUBMEIF  OF  XFDLIB
     JOINING  MEI      AS  PREFIX.
*抽出Ｆ
 FD  NYYOTEIF
                       LABEL     RECORD    IS   STANDARD
                       BLOCK  CONTAINS  6    RECORDS.
      COPY     NYYOTEIF OF  XFDLIB
     JOINING  NYY      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PRO             PIC  X(02).
     03  DSP-GRP             PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-FNC             PIC  X(04).
*ステータス
 01  STA-AREA.
     03  DSP-STA             PIC  X(02).
     03  HED-STA             PIC  X(02).
     03  BDY-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  NYY-STA             PIC  X(02).
*20220720
     03  MEI-STA             PIC  X(02).
*カウント
 01  CNT-AREA.
     03  IN-CNT                   PIC  9(07)  VALUE  ZERO.
     03  OT-CNT                   PIC  9(07)  VALUE  ZERO.
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"倉庫コードが違います".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"入荷予定日が違います".
     03  MSG-NO04            PIC  N(20)  VALUE
            NC"取消分が違います".
     03  MSG-NO05            PIC  N(20)  VALUE
            NC"対象データはありません".
     03  MSG-NO06            PIC  N(20)  VALUE
            NC"対象データ抽出中です".
     03  MSG-NO07            PIC  N(20)  VALUE
            NC"開始が終了を越えています".
*20220720
     03  MSG-NO08            PIC  N(20)  VALUE
            NC"出力区分は　空白か１です".
 01  TBL-MSG-R   REDEFINES    MSG-TBL.
*20220720
*-   03  TBL-MSG             PIC  N(20)  OCCURS  7   TIMES.
     03  TBL-MSG             PIC  N(20)  OCCURS  8   TIMES.
*
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*月末
 01  WORK-AREA.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(02)  VALUE  ZERO.
     03  PF05-FLG            PIC  9(01)  VALUE  ZERO.
     03  HED-FLG             PIC  9(01)  VALUE  ZERO.
 01  WK-BDY-KEY.
     03  WK-BDY-F02          PIC  9(07).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### NVD0150I".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-JISOKCD          PIC  X(02).
*20220720
   01  LINK-OUT-OUTKBN       PIC  X(01).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-JISOKCD
*20220720
                                                LINK-OUT-OUTKBN.
******************************************************************
 DECLARATIVES.
*画面Ｆ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*発注Ｆ（ヘッダ）
 HED-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACHEDF.
     MOVE    "HACHEDL2"    TO    ERR-FL-ID.
     MOVE     HED-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*発注Ｆ（明細）
 BDY-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HACMEIF.
     MOVE    "HACMEIL1"    TO    ERR-FL-ID.
     MOVE     BDY-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
*条件Ｆ
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JYOKEN1.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*20220720
*SUB商品名称マスタ
 MEI-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       SUBMEIL1.
     MOVE    "SUBMEIL1"    TO    ERR-FL-ID.
     MOVE     MEI-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*抽出Ｆ
 NYY-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       NYYOTEIF.
     MOVE    "NYYOTEIF"    TO    ERR-FL-ID.
     MOVE     NYY-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
***************
     MOVE "01"  TO  LINK-SOKCD.
     MOVE "01"  TO  LINK-JISOKCD.
*
*****DISPLAY "倉庫CD    ="   LINK-SOKCD.
*****DISPLAY "代表倉庫CD="   LINK-JISOKCD.
***************
     DISPLAY  "**  NVD0150I   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  NVD0150I    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       HACHEDF   HACMEIF.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       JYOKEN1.
*20220720
     OPEN     INPUT       SUBMEIL1.
     OPEN     OUTPUT      NYYOTEIF.
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
*特販部名称編集
     MOVE      SPACE              TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     "99"                TO   JYO-F01.
     MOVE     "BUMON"             TO   JYO-F02.
     READ      JYOKEN1
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"     TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03              TO    HEN-TOKHAN
     END-READ.
*
*初期画面表示へ
     MOVE  1              TO  MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   4      PERFORM  UPDT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF.
     CLOSE      HACHEDF   HACMEIF.
     CLOSE      ZSOKMS1.
     CLOSE      JYOKEN1.
*20220720
     CLOSE      SUBMEIL1.
     CLOSE      NYYOTEIF.
*
     DISPLAY "* HACMEIF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* NYYOTEIF(OT)=" OT-CNT   " *" UPON CONS.
*
     IF   OT-CNT        =  ZERO
       MOVE  4050       TO  PROGRAM-STATUS
     END-IF.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FVD01501"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FVD01501.
     PERFORM  OPT-CLR-SEC.
*倉庫
     IF     LINK-JISOKCD    NOT =  "01"  AND  "88"
         MOVE   LINK-SOKCD      TO  SOK-F01   DSP-SOKOCD
         READ   ZSOKMS1
             INVALID
               CONTINUE
             NOT  INVALID
               MOVE   SOK-F02   TO  DSP-SOKONM
         END-READ
     END-IF.
*開始入荷予定日
     MOVE  WK-Y         TO  DSP-SNYUYY  DSP-ENYUYY.
     MOVE  WK-M         TO  DSP-SNYUMM  DSP-ENYUMM.
     MOVE  WK-D         TO  DSP-SNYUDD  DSP-ENYUDD.
*ＢＯＤＹ部入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力処理
*=============================================================
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*倉庫ＣＤプロテクト
     IF    LINK-JISOKCD    NOT =   "01"  AND  "88"
       MOVE  "X"          TO  EDIT-STATUS  OF  DSP-SOKOCD
     END-IF.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  9      TO  PF05-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC   SECTION.
*倉庫ＣＤ
     IF  LINK-JISOKCD    =   "01"  OR  "88"
         IF  DSP-SOKOCD  =   SPACE
             MOVE   NC"全倉庫"       TO  DSP-SOKONM
         ELSE
             MOVE   DSP-SOKOCD       TO  SOK-F01
             READ   ZSOKMS1
               INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   2        TO  ERR-FLG
                 END-IF
                 MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKOCD
                 MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKOCD
                 MOVE  SPACE      TO  DSP-SOKONM
               NOT  INVALID  KEY
                 MOVE  SOK-F02    TO  DSP-SOKONM
             END-READ
         END-IF
     END-IF.
*開始入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-SNYUYY          TO   WK-SYY.
     MOVE     DSP-SNYUMM          TO   WK-SMM.
     MOVE     DSP-SNYUDD          TO   WK-SDD.
     MOVE     WK-SYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-SYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-SDATE
     ELSE
         MOVE     ZERO            TO   WK-SDATE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
*終了入荷予定日
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     DSP-ENYUYY          TO   WK-EYY.
     MOVE     DSP-ENYUMM          TO   WK-EMM.
     MOVE     DSP-ENYUDD          TO   WK-EDD.
     MOVE     WK-EYMD             TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF  LINK-OUT-RET  =  ZERO
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE     WK-EYMD         TO   LINK-IN-YMD6
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE     LINK-OUT-YMD    TO   WK-EDATE
     ELSE
         MOVE     ZERO            TO   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   3        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-ENYUYY
     END-IF.
*開始・終了の関連チェック
     IF  WK-SDATE  NOT =  ZERO  AND
         WK-EDATE  NOT =  ZERO  AND
         WK-SDATE     >   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   7        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-SNYUYY
                                 EDIT-OPTION  OF  DSP-SNYUMM
                                 EDIT-OPTION  OF  DSP-SNYUDD
                                 EDIT-OPTION  OF  DSP-ENYUYY
                                 EDIT-OPTION  OF  DSP-ENYUMM
                                 EDIT-OPTION  OF  DSP-ENYUDD
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-SNYUYY
     END-IF.
*取消分
     IF  DSP-TORIKB  NOT =  SPACE  AND  1
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-TORIKB
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-TORIKB
     END-IF.
*20220720
*出力区分
     IF  ( DSP-OUTKBN  NOT =  SPACE ) AND
         ( DSP-OUTKBN  NOT =  "1"   )
         IF  ERR-FLG  =  ZERO
             MOVE   8        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-OUTKBN
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-OUTKBN
     END-IF.
     MOVE    DSP-OUTKBN      TO     LINK-OUT-OUTKBN.
*
 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*      2.2       確認入力処理
*=============================================================
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                     TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  9      TO  PF05-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
*=============================================================
*                確認入力チェック（対象データ存在ＣＨＫ）
*=============================================================
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*発注Ｆスタート
     MOVE  ZERO              TO  HED-FLG.
     MOVE  DSP-SOKOCD        TO  HED-F17.
     MOVE  ZERO              TO  HED-F02.
     START HACHEDF   KEY >=      HED-F17
                                 HED-F02
         INVALID
            MOVE   9         TO  HED-FLG
         NOT  INVALID
*発注Ｆリード
            PERFORM  HED-RD-SEC
     END-START.
*発注ファイル（明細）読み込み
     IF  HED-FLG  =  0
            PERFORM  DATA-GET-SEC
     END-IF.
*対象データ存在ＣＨＫ
     IF  HED-FLG  =  9
         MOVE    5    TO    ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE    6    TO    ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE    4    TO    MAIN-FLG
     ELSE
         MOVE    99   TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                発注ファイル（明細）検索
*=============================================================
 DATA-GET-SEC   SECTION.
     MOVE  HED-F02           TO  BDY-F02.
     MOVE  ZERO              TO  BDY-F03.
     START HACMEIF   KEY >=      BDY-F02
                                 BDY-F03
         INVALID
            MOVE   HIGH-VALUE    TO   WK-BDY-KEY
         NOT  INVALID
            PERFORM  BDY-RD-SEC
     END-START.
*
     IF  WK-BDY-KEY  NOT =   HED-F02
            PERFORM  HED-RD-SEC
            IF  HED-FLG  =  0
                GO       TO    DATA-GET-SEC
            END-IF
     END-IF.
 DATA-GET-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理
*=============================================================
 BDY-RD-SEC      SECTION.
*リード
     READ  HACMEIF    NEXT
         AT   END
           MOVE   HIGH-VALUE   TO  WK-BDY-KEY
           GO              TO  BDY-RD-EXIT
     END-READ.
     ADD   1               TO  IN-CNT.
*#2021/06/09 NAV ST 完納は出力しない
     IF  BDY-F05  =  ZERO
         CONTINUE
     ELSE
         GO                TO  BDY-RD-SEC
     END-IF.
*#2021/06/09 NAV ED 完納は出力しない
*
     MOVE   BDY-F02        TO  WK-BDY-F02.
 BDY-RD-EXIT.
     EXIT.
*=============================================================
*                更新処理
*=============================================================
 UPDT-SEC          SECTION.
*転送
     MOVE     SPACE     TO  NYY-REC.
     INITIALIZE             NYY-REC.
     MOVE     HED-F17   TO  NYY-F01.
     MOVE     HED-F06   TO  NYY-F02.
     MOVE     HED-F02   TO  NYY-F03.
     MOVE     BDY-F03   TO  NYY-F04.
     MOVE     HED-F11   TO  NYY-F05.
     MOVE     BDY-F06   TO  NYY-F06.
     MOVE     BDY-F07   TO  NYY-F07.
     MOVE     BDY-F08   TO  NYY-F08.
     MOVE     BDY-F09   TO  NYY-F09.
*#2021/06/16 NAV ST  発注数―入荷数をセット
     COMPUTE  NYY-F09  =  BDY-F09  -  BDY-F10.
     IF  BDY-F10  >  ZERO
     AND BDY-F09  >  BDY-F10
*20220720
*-       MOVE  "1"      TO  NYY-F99(24:1)
         MOVE  "1"      TO  NYY-F99(22:1)
     END-IF.
*#2021/06/16 NAV ED
     MOVE     HED-F15   TO  NYY-F10.
     MOVE     HED-F311  TO  NYY-F111.
     MOVE     HED-F312  TO  NYY-F112.
     EVALUATE HED-F01
         WHEN 50  MOVE NC"☆５０：通常仕入☆" TO  NYY-F112
         WHEN 51  MOVE NC"☆５１：仕入返品☆"   TO NYY-F112
                  COMPUTE NYY-F09 = NYY-F09 *  -1
         WHEN OTHER  MOVE SPACE           TO  NYY-F112
     END-EVALUATE.
     MOVE     BDY-F22       TO  NYY-F12.
     MOVE     BDY-F21       TO  NYY-F13.
*20220720 商品カテゴリ
     MOVE     BDY-F06       TO  MEI-F011.
     MOVE     BDY-F07       TO  MEI-F012.
     READ     SUBMEIL1
         INVALID
              MOVE SPACE    TO  NYY-F14
         NOT INVALID
              MOVE MEI-F09  TO  NYY-F14
     END-READ.
*抽出Ｆ更新
     WRITE    NYY-REC.
     ADD      1         TO  OT-CNT.
*発注Ｆリード
     PERFORM   BDY-RD-SEC.
     IF        WK-BDY-KEY  NOT =  HED-F02
               PERFORM   HED-RD-SEC
         IF    HED-FLG  NOT =  9
               PERFORM   DATA-GET-SEC
         END-IF
     END-IF.
     IF        HED-FLG  =  9
               MOVE     99      TO   MAIN-FLG
     END-IF.
 UPDT-EXIT.
     EXIT.
*=============================================================
*                発注Ｆリード処理
*=============================================================
 HED-RD-SEC      SECTION.
*リード
     READ  HACHEDF    NEXT
         AT   END
           MOVE   9        TO  HED-FLG
           GO              TO  HED-RD-EXIT
     END-READ.
 HED-RD-010.
*倉庫ＣＤ
     IF   DSP-SOKOCD  NOT =   SPACE   AND
          DSP-SOKOCD  NOT =   HED-F17
         MOVE   9          TO  HED-FLG
         GO                TO  HED-RD-EXIT
     END-IF.
 HED-RD-020.
*伝票区分
*#2020/09/03 NAV ST 伝区＝５０、５１を出力対象とする。
*****IF   HED-F01  NOT =  50
*        GO                TO  HED-RD-SEC
*****END-IF.
     IF   HED-F01  =  50  OR  51
          CONTINUE
     ELSE
         GO                TO  HED-RD-SEC
     END-IF.
*#2020/09/03 NAV ED
 HED-RD-030.
*枝番，完了区分
     IF   HED-F03  NOT =  ZERO
     OR   HED-F04  NOT =  ZERO
         GO                TO  HED-RD-SEC
     END-IF.
 HED-RD-040.
*納入予定日
     IF   HED-F11     <    WK-SDATE    OR
          HED-F11     >    WK-EDATE
         GO                TO  HED-RD-SEC
     END-IF.
 HED-RD-050.
*取消分
     EVALUATE    DSP-TORIKB
       WHEN       " "
          IF   HED-F24   NOT =  ZERO
             GO                TO  HED-RD-SEC
          END-IF
       WHEN       "1"
          IF   HED-F24   NOT =  1
             GO                TO  HED-RD-SEC
          END-IF
     END-EVALUATE.
 HED-RD-060.
*発注ＦＬＧ
     IF   HED-F25  NOT  =  1
         GO                TO  HED-RD-SEC
     END-IF.
 HED-RD-EXIT.
     EXIT.
*=============================================================
*                画面表示処理
*=============================================================
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  TBL-MSG(ERR-FLG)    TO  DSP-ERRMSG
     END-IF.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     MOVE  HEN-DATE              TO  DSP-SDATE.
     MOVE  HEN-TIME              TO  DSP-STIME.
     MOVE  HEN-TOKHAN-AREA       TO  DSP-TOKHAN.
     WRITE DSP-FVD01501.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
*=============================================================
*                画面読込処理
*=============================================================
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PRO.
     READ  DSPF.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
*=============================================================
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-SOKOCD
                             EDIT-CURSOR  OF  DSP-SNYUYY
                             EDIT-CURSOR  OF  DSP-SNYUMM
                             EDIT-CURSOR  OF  DSP-SNYUDD
                             EDIT-CURSOR  OF  DSP-ENYUYY
                             EDIT-CURSOR  OF  DSP-ENYUMM
                             EDIT-CURSOR  OF  DSP-ENYUDD
*20220720
                             EDIT-CURSOR  OF  DSP-OUTKBN
                             EDIT-CURSOR  OF  DSP-TORIKB.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-SOKOCD
                             EDIT-OPTION  OF  DSP-SNYUYY
                             EDIT-OPTION  OF  DSP-SNYUMM
                             EDIT-OPTION  OF  DSP-SNYUDD
                             EDIT-OPTION  OF  DSP-ENYUYY
                             EDIT-OPTION  OF  DSP-ENYUMM
                             EDIT-OPTION  OF  DSP-ENYUDD
*20220720
                             EDIT-OPTION  OF  DSP-OUTKBN
                             EDIT-OPTION  OF  DSP-TORIKB.
 OPT-CLR-EXIT.
     EXIT.
