****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　                        *
*    業務名　　　　　　　：　取引先マスタ　　　　　　　        *
*    モジュール名　　　　：　取引先マスタ保守　                *
*    作成日／作成者　　　：　92/11/10  /Y.Y                    *
*    更新日／更新者　　　：　09/03/12  /S.IMAI                 *
*    更新内容　　　　　　：　新郵便番号追加                    *
*    　　　　　　　　　　　　代表倉庫（倉庫マスタＲＥＡＤ）追加*
*    　　　　　　　　　　　　自社指定伝票タイプ                *
*    　　　　　　　　　　　　税計算区分　　　　　　　　　　　　*
*                            部門追加20090930                  *
*    処理概要　　　　　　：　ORIGINAL SMNT012→ＩＴ統制対応　
*    更新内容　　　　　　：　出荷オ、出荷手区分追加 2011/04/14 *
*                            (T.TAKAHASHI)                     *
*                　　　　：　シーズン開始日の追加　 2011/12/07 *
*                            (T.MIURA)                         *
*    更新日／更新者　　　：　2012/10/31  TAKEI                 *
*    更新内容　　　　　　：　流通ＢＭＳ対応                    *
*    更新日／更新者　　　：　2013/12/05  INOUE                 *
*    更新内容　　　　　　：　消費税増税対応                    *
*    更新日／更新者　　　：　2013/12/24  INOUE                 *
*    更新内容　　　　　　：　消費税　不要項目カット＆追加      *
*    更新日／更新者　　　：　2014/02/10  TAKAHASHI             *
*    更新内容　　　　　　：　税抜計算区分　チェック方法を変更  *
*    更新日／更新者　　　：　2017/08/29  TAKAHASHI             *
*    更新内容　　　　　　：　伝票纏め区分追加　　　　　　　　  *
*    更新日／更新者　　　：　2019/03/15 NAV INOUE              *
*    更新内容　　　　　　：　S11300660                         *
*    　　　　　　　　　　　　マスタ更新履歴ファイル項目追加　　*
*    更新日／更新者　　　：　2019/09/27  TAKAHASHI             *
*    更新内容　　　　　　：　消費税軽減税率対応　　　　　　　  *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0032I.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/12.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-150SI.
 OBJECT-COMPUTER.          K-150SI.
 SPECIAL-NAMES.
     STATION     IS        STA
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        MSTLOGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F04
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-ST.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*画面ファイル*
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
                           SYMBOLIC  DESTINATION        "DSP"
                           DESTINATION-1       DSP-WS
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING  MODE    DSP-PRO
                           UNIT      CONTROL   DSP-UNIT
                           SELECTED  FUNCTION  DSP-FNC
                           FILE      STATUS    DSP-ST.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*ＤＩＳＰＬＡＹ
 FD  DSPFILE.
     COPY        FIT00302 OF        XMDLIB.
*
 01  FM012R.
*****03  D-SYORI           PIC X(06).
     03  D-SYORI           PIC X(34).
*****03  D-KOMOKU          PIC X(540).
     03  D-KOMOKU          PIC X(547).
     03  D-MSG             PIC X(110).
     03  D-R002            PIC X(06).
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  DSP-AREA.
     03  DSP-FMT           PIC X(08) VALUE     SPACE.
     03  DSP-GRP           PIC X(08) VALUE     SPACE.
     03  DSP-WS            PIC X(08) VALUE     SPACE.
     03  DSP-WSR           REDEFINES DSP-WS.
         05  DSP-WS1       PIC X(02).
         05  DSP-WS2       PIC 9(03).
         05  DSP-WS3       PIC X(01).
     03  DSP-PRO           PIC X(02) VALUE     SPACE.
     03  DSP-UNIT          PIC X(06) VALUE     SPACE.
     03  DSP-FNC           PIC X(04) VALUE     SPACE.
     03  DSP-ST            PIC X(02) VALUE     SPACE.
     03  DSP-ST1           PIC X(04) VALUE     SPACE.
 01  MSG-AREA.
     03  MSG01             PIC N(20) VALUE
                           NC"登録されていません".
     03  MSG02             PIC N(20) VALUE
                           NC"すでに登録されています".
     03  MSG03             PIC N(20) VALUE
                           NC"正式名を入力してください".
     03  MSG04             PIC N(20) VALUE
                           NC"伝発区分エラー".
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"税端数区分エラー".
     03  MSG07             PIC N(20) VALUE
                           NC"ＣＨＫＤ区分エラー".
     03  MSG08             PIC N(20) VALUE
                           NC"締日を入力してください".
     03  MSG09             PIC N(20) VALUE
                           NC"取引先ＣＤを入力してください".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG11             PIC N(20) VALUE
                           NC"Ｙで入力してください".
     03  MSG12             PIC N(20) VALUE
                           NC"ＣＨＫＤ２区分エラー".
     03  MSG13             PIC N(20) VALUE
                           NC"付番区分エラー".
     03  MSG14             PIC N(20) VALUE
                           NC"倉庫マスタに存在しません".
     03  MSG15             PIC N(20) VALUE
                           NC"自社指定伝票タイプエラー".
     03  MSG16             PIC N(20) VALUE
                           NC"税計算区分エラー".
     03  MSG17             PIC N(20) VALUE
                           NC"売価チェック区分エラー".
     03  MSG18             PIC N(20) VALUE
                           NC"管理区分エラー".
     03  MSG19             PIC N(20) VALUE
                           NC"部門ＣＤエラー".
     03  MSG20             PIC N(20) VALUE
                     NC"部門変更日入力の場合は変更部門必須！！".
     03  MSG21             PIC N(20) VALUE
                     NC"指定された変更部門はマスタ未登録です。".
     03  MSG22             PIC N(20) VALUE
                     NC"部門変更日　日付論理エラーです。".
     03  MSG23             PIC N(20) VALUE
                   NC"変更日は、本日以降の日付を指定して下さい".
     03  MSG24             PIC N(20) VALUE
                           NC"出荷オンライン区分エラー".
     03  MSG25             PIC N(20) VALUE
                           NC"出荷手書区分エラー".
     03  MSG26             PIC N(20) VALUE
                           NC"シーズン開始春（月日）エラー".
     03  MSG27             PIC N(20) VALUE
                           NC"シーズン開始秋（月日）エラー".
     03  MSG28             PIC N(20) VALUE
                           NC"発注集計表区分エラー".
**  ADD  2012/11/01
     03  MSG29             PIC N(20) VALUE
                           NC"出場特定区分エラー".
*2013/12/05↓
     03  MSG30   PIC N(20) VALUE
                 NC"指定された値が違います".
     03  MSG31   PIC N(20) VALUE
                 NC"計上税区分が違います。".
     03  MSG32   PIC N(20) VALUE
                 NC"改訂日は未来日付を入力してください".
     03  MSG33   PIC N(20) VALUE
                 NC"日付が不正です。".
*2013/12/25↓
     03  MSG34   PIC N(20) VALUE
                 NC"改訂日を入力してください。".
*2013/12/25↑
*2013/12/05↑
*2017/08/29↓
     03  MSG35   PIC N(20) VALUE
                 NC"伝票纏め区分を正しく入力して下さい。".
*2017/08/29↑
*2019/09/27↓
     03  MSG36   PIC N(20) VALUE
                 NC"請求区分を正しく入力して下さい。".
*2019/09/27↑
*
     03  PMSG01            PIC N(20) VALUE
                           NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_次検索".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_再入力".
*01  SYS-DATE              PIC 9(08).
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  HJYOKEN-INV-FLG   PIC X(03) VALUE     SPACE.
*2013/12/05↓
     03  JYO-INVALID-FLG   PIC 9(01) VALUE     ZERO.
     03  WK-TOK-F22        PIC 9(09) VALUE     ZERO.
*2013/12/25↓
     03  WK-TOK-F20        PIC 9(11) VALUE     ZERO.
*2013/12/25↑
*2013/12/05↑
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
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
     03  MSL-ST            PIC X(02).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"得意先マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
     03  FILE-ERR4         PIC N(12) VALUE
                                  NC"マスタ更新履歴Ｆ異常！".
     03  FILE-ERR5         PIC N(12) VALUE
                                  NC"条件ファイル異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
*↓2019.03.15
 01  BEFORE-DATA           PIC  X(543).
*↑2019.03.15
 01  DATA-TAIHI            PIC  X(543).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
******************************************************************
 PROCEDURE                 DIVISION USING PARA-BUMONCD
                                          PARA-TANCD
                                          PARA-UPDTDATE
                                          PARA-UPDTIME.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTOKMS.
     DISPLAY     TOK-ST    UPON      STA.
**** DISPLAY     TOK-ST1   UPON      STA.
     DISPLAY     FILE-ERR1 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      STA.
**** DISPLAY     TOK-ST1   UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HJYOKEN.
     DISPLAY     JYO-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ST    UPON      STA.
     DISPLAY     FILE-ERR4 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
**** DISPLAY     DSP-ST1   UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM     END-SEC.
     STOP        RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC                  SECTION.
     OPEN        I-O       HTOKMS  MSTLOGF
                           DSPFILE.
     OPEN        INPUT     ZSOKMS  HJYOKEN.
*
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
*
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        TOK-REC.
     INITIALIZE  TOK-REC.
*受渡しパラメタセット
     MOVE     DATE-AREA    TO        PARA-UPDTDATE.
     MOVE     WK-TIME(1:6) TO        PARA-UPDTIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-INIT-SEC.
 MAIN-010.
     MOVE        PMSG01    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SYORI"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE     9      TO        END-FLG
           GO              TO        MAIN-EXIT
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*処理区分 CHK
     IF  (SYORI  =  1 OR 2 OR 3)
         MOVE    SPACE     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"        TO        EDIT-OPTION  OF  SYORI
         MOVE    SPACE     TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
       ELSE
         MOVE    MSG05     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYORI
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-010.
 MAIN-020.
*--< ＰＦキー選択 >--*
     IF  (SYORI  =  2)
           MOVE  PMSG02    TO        MSG2
     ELSE
           MOVE  PMSG03    TO        MSG2
     END-IF.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           GO              TO        MAIN-SEC
       WHEN
        "F010"
           IF  (SYORI  =  2)  AND  (ZI-FLG = ZERO)
               IF  (INV-SW = 1)
                   MOVE        TORICD    TO        TOK-F01
                   START     HTOKMS  KEY IS  >   TOK-F01
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  HTOKMS    NEXT
                 AT  END
                   MOVE NC"次レコード無し"     TO        MSG1
                   MOVE   "MSG1"     TO        DSP-GRP
                   PERFORM DSP-WR-SEC
                   MOVE    1         TO        ZI-FLG
                   GO                TO        MAIN-020
                 NOT AT END
                   MOVE    ZERO      TO        INV-SW
                   GO      TO        MAIN-000
               END-READ
           END-IF
           IF  (SYORI  =  2)
                MOVE NC"次レコード無し"     TO        MSG1
           ELSE
                MOVE   MSG10          TO        MSG1
           END-IF
           MOVE       "MSG1"          TO        DSP-GRP
           PERFORM     DSP-WR-SEC
           GO                         TO        MAIN-020
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG10   TO        MSG1
           MOVE   "MSG1"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO              TO        MAIN-020
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*取引先コード CHK
 MAIN-001.
     IF  (TORICD  =  ZERO)
         MOVE    MSG09     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC.
*
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE        SPACE     TO        MSG1.
     MOVE       "MSG1"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*得意先マスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TEN1-SEC
         MOVE       "SCREEN"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC
*        MOVE       "GRP001"   TO        DSP-GRP
*        PERFORM     DSP-WR-SEC.
     IF  (SYORI  =  3)
         GO                TO        MAIN-040.
 MAIN-030.
     MOVE        PMSG03    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                             MOVE    SPACE   TO  D-KOMOKU  D-R002
                             PERFORM ZOKUSEI-CLR-SEC
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU  YUBIN
                                             NYBN1   NYBN2
                                             JYU1    JYU2    TEL
                                             FAX     SIME1   SIME2
                                             SIME3   KUBUN1 KUBUN2
                                             KUBUN3  KUBUN4 KUBUN5
                                             KUBUN6  KUBUN7 KUBUN8
                                             GENKA   BAIKA   R002
                                             FDENNO  FKAINO FENDNO
                                             UDENNO  UKAINO UENDNO
                                             SOKOCD  SOKONM DENCD
                                             ZEIKBN  KANRI
                                             SYUONL  SYUTEG
                                             BUMON   BUMNM HCHSYU
                                             BUMON2  BUMNM2 BUMHEN
                                             HARU1 HARU2 AKI1 AKI2
*** 2012/10/31
                                             STOKBN
*2013/12/05↓
                                             ZEIA
                                             ZEIN
*2013/12/24↓
*                                            ZEIKB1
*                                            ZEINM1
*                                            CHGDAY
*                                            ZEIKB2
*                                            ZEINM2
*2013/12/24↑
*2013/12/25↓
                                             CHGDAY
                                             ZEIN2
*2013/12/25↑
*2013/12/05↑
*#2017/08/29 NAV ST
                                             MATOME
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST
                                             SEIKBN
*#2019/09/27 NAV ED
                             GO        TO   MAIN-020
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
     MOVE     SPACE          TO   MSG1.
     MOVE     "MSG1"         TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM  DSP-WR-SEC.
*正式名
     IF  (TNAME  =  SPACE)
         MOVE     "R"        TO   EDIT-OPTION  OF  TNAME
         MOVE     "C"        TO   EDIT-CURSOR  OF  TNAME
         MOVE      MSG03     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  TNAME
         MOVE      SPACE     TO   EDIT-CURSOR  OF  TNAME
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*締日（１）
     IF  (SIME1  =  0 )
         MOVE     "R"        TO   EDIT-OPTION  OF  SIME1
         MOVE     "C"        TO   EDIT-CURSOR  OF  SIME1
         MOVE      MSG08     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  SIME1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SIME1
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*伝発区分
     IF  (KUBUN1  =  0 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN1
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN1
         MOVE      MSG04     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*付番区分
     IF  (KUBUN7  =  0 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN7
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN7
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN7
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN7
         MOVE      MSG13     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*2004/06/22 NAV START*
*税計算区分
     IF  (ZEIKBN  =  "0" OR "9")
         MOVE     "M"        TO   EDIT-OPTION  OF  ZEIKBN
         MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIKBN
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  ZEIKBN
         MOVE     "C"        TO   EDIT-CURSOR  OF  ZEIKBN
         MOVE      MSG16     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*2004/06/22 NAV END  *
*2004/07/16 NAV START*
*売価チェック区分
     IF  (BCHK  =  "0" OR "9")
         MOVE     "M"        TO   EDIT-OPTION  OF  BCHK
         MOVE      SPACE     TO   EDIT-CURSOR  OF  BCHK
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  BCHK
         MOVE     "C"        TO   EDIT-CURSOR  OF  BCHK
         MOVE      MSG17     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*2004/06/22 NAV END  *
*税端数区分
     IF  (KUBUN2  =  0 OR 4 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN2
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN2
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN2
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN2
         MOVE      MSG06     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*ＣＨＫＤ区分
*2001/07/31  モジュラース１０（ウエイト１．３）追加の為
*2019/06/18  モジュラース１０（ウエイト３）追加の為
     IF  (KUBUN5  =  0 OR 1 OR 2 OR 3 OR 5 OR 6 OR 7 OR 8)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN5
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN5
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN5
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN5
         MOVE      MSG07     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*ＣＨＫＤ２区分
*2001/07/31  モジュラース１０（ウエイト１．３）追加の為
*2019/06/18  モジュラース１０（ウエイト３）追加の為
     IF  (KUBUN6  =  0 OR 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN6
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN6
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN6
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN6
         MOVE      MSG12     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
******************* 99/09/27追加 *******************************
*倉庫マスタＣＨＫ
     MOVE      SOKOCD         TO   SOK-F01.
     READ      ZSOKMS
       INVALID
               MOVE   SPACE       TO  SOKONM
               MOVE     "R"        TO   EDIT-OPTION  OF  SOKOCD
               MOVE     "C"        TO   EDIT-CURSOR  OF  SOKOCD
               MOVE      MSG14     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
       NOT INVALID
               MOVE  SOK-F02      TO  SOKONM
               MOVE     "M"        TO   EDIT-OPTION  OF  SOKOCD
               MOVE      SPACE     TO   EDIT-CURSOR  OF  SOKOCD
               MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-READ.
****************************************************************
******************* 99/11/02追加 *******************************
*自社指定伝票タイプチェック
     IF        DENCD    NOT    NUMERIC
               MOVE     ZERO       TO   DENCD
     END-IF.
     IF        DENCD  =  0 OR 1 OR 2 OR 3 OR 4
               MOVE     "M"        TO   EDIT-OPTION OF DENCD
               MOVE     SPACE      TO   EDIT-CURSOR OF DENCD
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF DENCD
               MOVE     "C"        TO   EDIT-CURSOR OF DENCD
               MOVE      MSG15     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
******************* 09/08/19追加 *******************************
*管理区分
     IF  KANRI   NOT  =     SPACE
         IF  KANRI   NOT  = "1"
             MOVE     "R"        TO   EDIT-OPTION  OF  KANRI
             MOVE     "C"        TO   EDIT-CURSOR  OF  KANRI
             MOVE      MSG18     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         ELSE
             MOVE     "M"        TO   EDIT-OPTION  OF  KANRI
             MOVE      SPACE     TO   EDIT-CURSOR  OF  KANRI
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-IF
     ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  KANRI
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KANRI
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
****************************************************************
******************* 11/04/14追加 *******************************
*出荷オンライン区分
     IF  SYUONL  NOT  =     SPACE
         IF  SYUONL  NOT  = "1"
             MOVE     "R"        TO   EDIT-OPTION  OF  SYUONL
             MOVE     "C"        TO   EDIT-CURSOR  OF  SYUONL
             MOVE      MSG24     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         ELSE
             MOVE     "M"        TO   EDIT-OPTION  OF  SYUONL
             MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUONL
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-IF
     ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  SYUONL
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUONL
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
****************************************************************
******************* 11/04/14追加 *******************************
*出荷手書区分
     IF  SYUTEG  NOT  =     SPACE
         IF  SYUTEG  NOT  = "1"
             MOVE     "R"        TO   EDIT-OPTION  OF  SYUTEG
             MOVE     "C"        TO   EDIT-CURSOR  OF  SYUTEG
             MOVE      MSG25     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         ELSE
             MOVE     "M"        TO   EDIT-OPTION  OF  SYUTEG
             MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUTEG
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-IF
     ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  SYUTEG
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUTEG
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
****************************************************************
******************* 09/08/19追加 *******************************
*部門ＣＤ
     IF  (BUMON  NOT  NUMERIC)
     OR  (BUMON  =    ZERO   )
         MOVE     "R"        TO   EDIT-OPTION  OF  BUMON
         MOVE     "C"        TO   EDIT-CURSOR  OF  BUMON
         MOVE      MSG19     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
*********部門登録チェック
         MOVE    BUMON     TO    JYO-F02
         PERFORM   HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = "INV"
             MOVE     "R"        TO   EDIT-OPTION  OF  BUMON
             MOVE     "C"        TO   EDIT-CURSOR  OF  BUMON
             MOVE      MSG19     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE  ALL NC"＊"    TO   BUMNM
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         END-IF
         MOVE     "M"        TO   EDIT-OPTION  OF  BUMON
         MOVE      SPACE     TO   EDIT-CURSOR  OF  BUMON
         MOVE      JYO-F03   TO   BUMNM
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*部門変更日
     IF  BUMHEN NOT NUMERIC
         MOVE  ZERO     TO   BUMHEN
         MOVE "M"      TO EDIT-OPTION OF BUMHEN
         MOVE  SPACE   TO EDIT-CURSOR OF BUMHEN
         MOVE "GRP001" TO DSP-GRP
         PERFORM   DSP-WR-SEC
     ELSE
         IF   BUMHEN  NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     BUMHEN         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   "R"        TO EDIT-OPTION  OF  BUMHEN
                   MOVE   "C"        TO EDIT-CURSOR  OF  BUMHEN
                   MOVE    MSG22     TO MSG1
                   MOVE   "MSG1"     TO DSP-GRP
                   PERFORM   DSP-WR-SEC
                   MOVE   "GRP001"   TO DSP-GRP
                   PERFORM   DSP-WR-SEC
                   GO   TO   MAIN-030
              ELSE
                   IF   SYS-DATE > BUMHEN
                        MOVE "R"    TO EDIT-OPTION OF BUMHEN
                        MOVE "C"    TO EDIT-CURSOR OF BUMHEN
                        MOVE  MSG23 TO MSG1
                        MOVE "MSG1" TO DSP-GRP
                        PERFORM   DSP-WR-SEC
                        MOVE "GRP001" TO DSP-GRP
                        PERFORM   DSP-WR-SEC
                        GO   TO   MAIN-030
                   ELSE
                        MOVE "M"      TO EDIT-OPTION OF BUMHEN
                        MOVE  SPACE   TO EDIT-CURSOR OF BUMHEN
                   END-IF
              END-IF
     END-IF.
*変更部門ＣＤ
     IF  ((BUMON2 = ZERO ) OR (BUMON2 NOT NUMERIC))
     AND (BUMHEN  NOT =  ZERO)
         MOVE     "R"        TO   EDIT-OPTION  OF  BUMON2
         MOVE     "R"        TO   EDIT-OPTION  OF  BUMHEN
         MOVE     "C"        TO   EDIT-CURSOR  OF  BUMON2
         MOVE      MSG20     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     ELSE
*********部門登録チェック
         IF  BUMHEN  NOT =  ZERO
             MOVE    BUMON2    TO    JYO-F02
             PERFORM   HJYOKEN-READ-SEC
             IF  HJYOKEN-INV-FLG = "INV"
                 MOVE     "R"        TO   EDIT-OPTION  OF  BUMON2
                 MOVE     "C"        TO   EDIT-CURSOR  OF  BUMON2
                 MOVE      MSG21     TO   MSG1
                 MOVE     "MSG1"     TO   DSP-GRP
                 PERFORM   DSP-WR-SEC
                 MOVE  ALL NC"＊"    TO   BUMNM
                 MOVE     "GRP001"   TO   DSP-GRP
                 PERFORM   DSP-WR-SEC
                 GO   TO   MAIN-030
             END-IF
             MOVE     "M"        TO   EDIT-OPTION  OF  BUMON2
             MOVE     "M"        TO   EDIT-OPTION  OF  BUMHEN
             MOVE      SPACE     TO   EDIT-CURSOR  OF  BUMON2
             MOVE      JYO-F03   TO   BUMNM2
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-IF
     END-IF.
*シーズン開始日（春）
     IF  HARU1   NOT  =     SPACE
         IF  ((HARU1   =  0) OR  (HARU1   >  12))
         OR  ((HARU2 NOT NUMERIC) OR (HARU2 = 0) OR (HARU2 > 31))
             MOVE     "R"        TO   EDIT-OPTION  OF  HARU1
             MOVE     "R"        TO   EDIT-OPTION  OF  HARU2
             MOVE     "C"        TO   EDIT-CURSOR  OF  HARU1
             MOVE      MSG26     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         ELSE
*シーズン開始日（秋）
             IF ((AKI1 NOT NUMERIC) OR(AKI1 = 0) OR (AKI1 > 12))
             OR ((AKI2 NOT NUMERIC) OR (AKI2 =  0) OR (AKI2 > 31))
                   MOVE   "R"   TO   EDIT-OPTION  OF  AKI1
                   MOVE   "R"   TO   EDIT-OPTION  OF  AKI2
                   MOVE   "C"   TO   EDIT-CURSOR  OF  AKI1
                   MOVE      MSG27     TO   MSG1
                   MOVE     "MSG1"     TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   MOVE     "GRP001"   TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   GO   TO   MAIN-030
             ELSE
                 MOVE  "M"        TO   EDIT-OPTION  OF  HARU1
                 MOVE  "M"        TO   EDIT-OPTION  OF  HARU2
                 MOVE  "M"        TO   EDIT-OPTION  OF  AKI1
                 MOVE  "M"        TO   EDIT-OPTION  OF  AKI2
                 MOVE   SPACE     TO   EDIT-CURSOR  OF  HARU1
                 MOVE  "GRP001"   TO   DSP-GRP
                 PERFORM   DSP-WR-SEC
             END-IF
         END-IF
     ELSE
        IF ((HARU2 = SPACE) AND (AKI1 = SPACE) AND (AKI2 = SPACE))
                 MOVE  "M"        TO   EDIT-OPTION  OF  HARU1
                 MOVE  "M"        TO   EDIT-OPTION  OF  HARU2
                 MOVE  "M"        TO   EDIT-OPTION  OF  AKI1
                 MOVE  "M"        TO   EDIT-OPTION  OF  AKI2
                 MOVE   SPACE     TO   EDIT-CURSOR  OF  HARU1
                 MOVE  "GRP001"   TO   DSP-GRP
                 PERFORM   DSP-WR-SEC
        ELSE
             MOVE     "R"        TO   EDIT-OPTION  OF  HARU1
             MOVE     "R"        TO   EDIT-OPTION  OF  HARU2
             MOVE     "C"        TO   EDIT-CURSOR  OF  HARU1
             MOVE      MSG26     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
        END-IF
     END-IF.
******************* 12/03/05追加 *******************************
*発注集計表区分
     IF  HCHSYU  NOT  =     SPACE
         IF  HCHSYU  NOT  = "1"
             MOVE     "R"        TO   EDIT-OPTION  OF  HCHSYU
             MOVE     "C"        TO   EDIT-CURSOR  OF  HCHSYU
             MOVE      MSG25     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         ELSE
             MOVE     "M"        TO   EDIT-OPTION  OF  HCHSYU
             MOVE      SPACE     TO   EDIT-CURSOR  OF  HCHSYU
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
         END-IF
     ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  HCHSYU
         MOVE      SPACE     TO   EDIT-CURSOR  OF  HCHSYU
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
******************2012/11/01追加 *******************************
*出場特定チェック
     IF        STOKBN   NOT    NUMERIC
               MOVE     ZERO       TO   STOKBN
     END-IF.
     IF        STOKBN =  0 OR 1 OR 2 OR 3
               MOVE     "M"        TO   EDIT-OPTION OF STOKBN
               MOVE     SPACE      TO   EDIT-CURSOR OF STOKBN
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF STOKBN
               MOVE     "C"        TO   EDIT-CURSOR OF STOKBN
               MOVE      MSG29     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*2013/12/05↓
*税扱い区分チェック
     IF        ZEIA     NOT    NUMERIC
               MOVE     ZERO       TO   ZEIA
     END-IF.
     IF        ZEIA   =  1 OR 2
               MOVE     "M"        TO   EDIT-OPTION OF ZEIA
               MOVE     SPACE      TO   EDIT-CURSOR OF ZEIA
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF ZEIA
               MOVE     "C"        TO   EDIT-CURSOR OF ZEIA
               MOVE      MSG30     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*売上計上時税抜計算区分チェック
*    IF        ZEIN     NOT    NUMERIC
*              MOVE     ZERO       TO   ZEIN
*    END-IF.
     IF        ZEIN   = "0" OR "1"
               MOVE     "M"        TO   EDIT-OPTION OF ZEIN
               MOVE     SPACE      TO   EDIT-CURSOR OF ZEIN
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF ZEIN
               MOVE     "C"        TO   EDIT-CURSOR OF ZEIN
               MOVE      MSG30     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*****2014/02/10 T.T ST
*改訂後税抜計算区分チェック＊以下修正
     IF        ZEIN2  = "0" OR "1" OR SPACE
***************改訂後税抜計算区分が空白の場合
***************　改定後税区分に１、改定日に０をセットする。
               IF  ZEIN2  =  SPACE
                   MOVE  ZEIN      TO   ZEIN2
                   MOVE  ZERO      TO   CHGDAY
               END-IF
               MOVE     "M"        TO   EDIT-OPTION OF ZEIN2
               MOVE     SPACE      TO   EDIT-CURSOR OF ZEIN2
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF ZEIN2
               MOVE     "C"        TO   EDIT-CURSOR OF ZEIN2
               MOVE      MSG30     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*****2014/02/10 T.T ED
*2013/12/24↓
*計上税区分チェック
*    MOVE    50               TO   JYO-F01.
*    MOVE    ZEIKB1           TO   JYO-F02.
*    PERFORM HJYOKEN-READ-SUB.
*    IF      JYO-INVALID-FLG  =    1
*            MOVE     SPACE      TO   ZEINM1
*            MOVE     "R"        TO   EDIT-OPTION OF ZEIKB1
*            MOVE     "C"        TO   EDIT-CURSOR OF ZEIKB1
*            MOVE      MSG31     TO   MSG1
*            MOVE     "MSG1"     TO   DSP-GRP
*            PERFORM   DSP-WR-SEC
*            MOVE     "GRP001"   TO   DSP-GRP
*            PERFORM   DSP-WR-SEC
*            GO   TO   MAIN-030
*    ELSE
*            MOVE     JYO-F03    TO   ZEINM1
*            MOVE     "M"        TO   EDIT-OPTION OF ZEIKB1
*            MOVE     SPACE      TO   EDIT-CURSOR OF ZEIKB1
*            MOVE     "GRP001"   TO   DSP-GRP
*            PERFORM   DSP-WR-SEC
*    END-IF.
*2013/12/24↑
*税抜計算区分改定日チェック
*2013/12/25↓
*    改定後税抜区分が入力された場合は改定日入力必須
*****IF ( ZEIN2  NOT = " " ) AND
     IF ( ZEIN  NOT =  ZEIN2  ) AND
        ( CHGDAY = ZERO OR SPACE )
        MOVE     "R"        TO   EDIT-OPTION OF CHGDAY
        MOVE     "C"        TO   EDIT-CURSOR OF CHGDAY
        MOVE      MSG34     TO   MSG1
        MOVE     "MSG1"     TO   DSP-GRP
        PERFORM   DSP-WR-SEC
        MOVE     "GRP001"   TO   DSP-GRP
        PERFORM   DSP-WR-SEC
        GO   TO   MAIN-030
     END-IF.
*2013/12/25↑
*    常識チェック＆過去日はエラー
******2014/02/10 T.T ST
     IF   CHGDAY NOT NUMERIC
        MOVE     ZERO           TO   CHGDAY
     END-IF.
******2014/02/10 T.T ED
     IF ( CHGDAY NOT = ZERO  ) AND
        ( CHGDAY NOT = SPACE )
        MOVE     "2"            TO   LINK-IN-KBN
        MOVE     ZERO           TO   LINK-IN-YMD6
        MOVE     CHGDAY         TO   LINK-IN-YMD8
        MOVE     ZERO           TO   LINK-OUT-RET
        MOVE     ZERO           TO   LINK-OUT-YMD
        CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                        LINK-IN-YMD6
                                        LINK-IN-YMD8
                                        LINK-OUT-RET
                                        LINK-OUT-YMD
        IF   LINK-OUT-RET   = 9
             MOVE     "R"        TO   EDIT-OPTION OF CHGDAY
             MOVE     "C"        TO   EDIT-CURSOR OF CHGDAY
             MOVE      MSG33     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
        END-IF
        IF   DATE-AREA > CHGDAY
             MOVE     "R"        TO   EDIT-OPTION OF CHGDAY
             MOVE     "C"        TO   EDIT-CURSOR OF CHGDAY
             MOVE      MSG32     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
        ELSE
             MOVE     "M"        TO   EDIT-OPTION OF CHGDAY
             MOVE     SPACE      TO   EDIT-CURSOR OF CHGDAY
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
        END-IF
     END-IF.
     IF ( ZEIN2  = " " ) AND
        ( CHGDAY = ZERO OR SPACE )
          MOVE     "M"        TO   EDIT-OPTION OF CHGDAY
          MOVE     SPACE      TO   EDIT-CURSOR OF CHGDAY
          MOVE     "GRP001"   TO   DSP-GRP
          PERFORM   DSP-WR-SEC
     END-IF.
*2013/12/25↓
*改定後税抜計算区分チェック
*****2014/02/10 T.T ST
*T.T IF ( CHGDAY NOT = ZERO  ) AND
*T.T    ( CHGDAY NOT = SPACE )
*T.T    IF        ZEIN2  = "0" OR "1" OR SPACE
*T.T              MOVE     "M"        TO   EDIT-OPTION OF ZEIN2
*T.T              MOVE     SPACE      TO   EDIT-CURSOR OF ZEIN2
*T.T              MOVE     "GRP001"   TO   DSP-GRP
*T.T              PERFORM   DSP-WR-SEC
*T.T    ELSE
*T.T              MOVE     "R"        TO   EDIT-OPTION OF ZEIN2
*T.T              MOVE     "C"        TO   EDIT-CURSOR OF ZEIN2
*T.T              MOVE      MSG30     TO   MSG1
*T.T              MOVE     "MSG1"     TO   DSP-GRP
*T.T              PERFORM   DSP-WR-SEC
*T.T              MOVE     "GRP001"   TO   DSP-GRP
*T.T              PERFORM   DSP-WR-SEC
*T.T              GO   TO   MAIN-030
*T.T    END-IF
*T.T END-IF.
*****2014/02/10 T.T ED
*2013/12/25↑
*2013/12/24↓
*改訂後計上税区分チェック
*    IF    ( CHGDAY NOT = ZERO  ) AND
*          ( CHGDAY NOT = SPACE )
*       MOVE    50               TO   JYO-F01
*       MOVE    ZEIKB2           TO   JYO-F02
*       PERFORM HJYOKEN-READ-SUB
*       IF      JYO-INVALID-FLG  =    1
*               MOVE     SPACE      TO   ZEINM1
*               MOVE     "R"        TO   EDIT-OPTION OF ZEIKB2
*               MOVE     "C"        TO   EDIT-CURSOR OF ZEIKB2
*               MOVE      MSG31     TO   MSG1
*               MOVE     "MSG1"     TO   DSP-GRP
*               PERFORM   DSP-WR-SEC
*               MOVE     "GRP001"   TO   DSP-GRP
*               PERFORM   DSP-WR-SEC
*               GO   TO   MAIN-030
*       ELSE
*               MOVE      JYO-F03   TO   ZEINM2
*               MOVE      "M"       TO   EDIT-OPTION OF ZEIKB2
*               MOVE      SPACE     TO   EDIT-CURSOR OF ZEIKB2
*               MOVE      "GRP001"  TO   DSP-GRP
*               PERFORM   DSP-WR-SEC
*    ELSE
*               MOVE      ZERO      TO   ZEIKB2
*               MOVE      SPACE     TO   ZEINM2
*               MOVE      "M"       TO   EDIT-OPTION OF ZEIKB2
*               MOVE      SPACE     TO   EDIT-CURSOR OF ZEIKB2
*               MOVE      "GRP001"  TO   DSP-GRP
*               PERFORM   DSP-WR-SEC
*    END-IF.
*2013/12/24↑
*2013/12/05↑
****************************************************************
****************************************************************
*#2017/08/29 NAV ST 項目追加
*伝票纏め区分チェック
     IF        MATOME = SPACE OR "1"
               MOVE     "M"        TO   EDIT-OPTION OF MATOME
               MOVE     SPACE      TO   EDIT-CURSOR OF MATOME
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF MATOME
               MOVE     "C"        TO   EDIT-CURSOR OF MATOME
               MOVE      MSG35     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*#2017/08/29 NAV ED
****************************************************************
*#2019/09/27 NAV ST 項目追加
*請求区分
     IF        SEIKBN = SPACE OR "1"
               MOVE     "M"        TO   EDIT-OPTION OF SEIKBN
               MOVE     SPACE      TO   EDIT-CURSOR OF SEIKBN
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF SEIKBN
               MOVE     "C"        TO   EDIT-CURSOR OF SEIKBN
               MOVE      MSG36     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
*#2019/09/27 NAV ED
*
     MOVE     "Y"            TO   R002.
     MOVE     "R002"         TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
 MAIN-040.
*確認入力
     MOVE      PMSG04        TO   MSG2.
     MOVE     "MSG2"         TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "R002"         TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE    DSP-FNC
         WHEN      "F004"
                             MOVE    SPACE   TO  D-KOMOKU  D-R002
                             PERFORM ZOKUSEI-CLR-SEC
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU  YUBIN
                                             NYBN1   NYBN2  SOKOCD
                                             SOKONM  DENCD
                                             JYU1    JYU2    TEL
                                             FAX     SIME1   SIME2
                                             SIME3   KUBUN1 KUBUN2
                                             KUBUN3  KUBUN4 KUBUN5
                                             KUBUN6  KUBUN7 KUBUN8
                                             GENKA   BAIKA   R002
                                             UKAINO  UENDNO UDENNO
                                             FKAINO  FENDNO FDENNO
                                             ZEIKBN  KANRI
                                             SYUONL  SYUTEG
                                             BUMON   BUMNM HCHSYU
                                             BUMON2  BUMNM2 BUMHEN
                                             HARU1 HARU2 AKI1 AKI2
*** 2012/10/31
                                             STOKBN
*2013/12/05↓
                                             ZEIA
                                             ZEIN
*2013/12/24↓
*                                            ZEIKB1
*                                            ZEINM1
*                                            CHGDAY
*                                            ZEIKB2
*                                            ZEINM2
*2013/12/24↑
*2013/12/25↓
                                             CHGDAY
                                             ZEIN2
*2013/12/25↑
*2013/12/05↑
*#2017/08/29 NAV ST
                                             MATOME
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST
                                             SEIKBN
*#2019/09/27 NAV ED
                             GO          TO   MAIN-020
         WHEN      "F009"
           MOVE     SPACE    TO        R002
           MOVE    "R002"    TO        DSP-GRP
           PERFORM  DSP-WR-SEC
           IF  (SYORI  = 1 OR 2)
                             GO        TO   MAIN-030
           ELSE
                             GO        TO   MAIN-040
           END-IF
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO   TO   MAIN-040
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*
     IF  (R002  NOT =  "Y")
           MOVE    MSG11    TO   MSG1
           MOVE   "MSG1"    TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE    TO   MSG1
           MOVE   "MSG1"    TO   DSP-GRP
           PERFORM DSP-WR-SEC
     END-IF.
*
     IF  (SYORI  =  3)
           GO          TO   MAIN-080.
*画面から転送
     PERFORM  DSP-TEN2-SEC.
 MAIN-080.
     EVALUATE   SYORI
         WHEN     1
              MOVE     SYS-DATE   TO  TOK-F98
              MOVE     TOK-REC    TO  DATA-TAIHI
              WRITE    TOK-REC
              END-WRITE
         WHEN     2
              MOVE     TOK-REC    TO  DATA-TAIHI
              REWRITE   TOK-REC
              END-REWRITE
         WHEN     3
              MOVE     TOK-REC    TO  DATA-TAIHI
              DELETE    HTOKMS
              END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
*****MOVE     SPACE          TO      D-KOMOKU.
     MOVE     SPACE          TO      D-KOMOKU  D-R002.
     MOVE     SPACE          TO      BUMON     BUMON2.
     MOVE     ZERO           TO      BUMHEN.
     MOVE     SPACE          TO      BUMNM     BUMNM2.
*2013/12/05↓
     MOVE     SPACE          TO      ZEIN.
*2013/12/24↓
*    MOVE     ZERO           TO      CHGDAY    ZEIKB2.
*    MOVE     SPACE          TO      ZEINM1    ZEINM2.
*2013/12/24↑
*2013/12/25↓
     MOVE     ZERO           TO      CHGDAY.
     MOVE     SPACE          TO      ZEIN2.
*2013/12/25↑
*2013/12/05↑
*2019/09/27↓
     MOVE     SPACE          TO      SEIKBN.
*2019/09/27↓
     PERFORM  ZOKUSEI-CLR-SEC.
     MOVE    "SCREEN"        TO      DSP-GRP.
     PERFORM  DSP-WR-SEC.
     INITIALIZE      TORICD  TOKUCD  TNAME  TKANA   TRYAKU  YUBIN
                     NYBN1   NYBN2   SOKOCD SOKONM  DENCD  HCHSYU
                     JYU1    JYU2    TEL    FAX     SIME1   SIME2
                     SIME3   KUBUN1  KUBUN2 KUBUN3  KUBUN4  KUBUN5
                     KUBUN6  KUBUN7  KUBUN8 FKAINO  FENDNO  FDENNO
                     UKAINO  UENDNO  UDENNO GENKA   BAIKA   R002
                     ZEIKBN  KANRI  BUMON  BUMNM BUMON2 BUMNM2
*** 2012.10.31
                     STOKBN
                     BUMHEN  SYUONL SYUTEG HARU1 HARU2 AKI1 AKI2
*2013/12/05↓
                     ZEIA
                     ZEIN
*2013/12/24↓
*                    ZEIKB1
*                    ZEINM1
*                    CHGDAY
*                    ZEIKB2
*                    ZEINM2
*2013/12/24↑
*2013/12/25↓
                     CHGDAY
                     ZEIN2
*2013/12/25↑
*2013/12/05↑
*#2017/08/29 NAV ST
                     MATOME
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST
                     SEIKBN.
*#2019/09/27 NAV ED
     GO                      TO      MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HTOKMS
                       ZSOKMS  HJYOKEN
                       DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"               TO  DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面    ＷＲＩＴＥ                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE       HEN-DATE     TO  SDATE.
     MOVE       HEN-TIME     TO  STIME.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FIT00302.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FIT00302.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FIT00302"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FIT00302.
     INITIALIZE              FIT00302.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*            商品Ｍのレコードを画面に転送            *
****************************************************************
 DSP-TEN1-SEC              SECTION.
*↓2019.03.15
     MOVE     TOK-REC        TO        BEFORE-DATA.
*↑2019.03.15
     MOVE     TOK-F01        TO        TORICD.
     MOVE     TOK-F52        TO        TOKUCD.
*2009/09/30 部門CD追加 START *************************
     MOVE     TOK-F75        TO        BUMON.
     MOVE     TOK-F75        TO        JYO-F02.
*****部門名称取得
     PERFORM  HJYOKEN-READ-SEC.
     IF       HJYOKEN-INV-FLG =  SPACE
              MOVE  JYO-F03  TO        BUMNM
     ELSE
              MOVE ALL NC"＊" TO       BUMNM
     END-IF.
     MOVE     TOK-F76        TO        BUMHEN.
     MOVE     TOK-F77        TO        BUMON2.
     MOVE     TOK-F77        TO        JYO-F02.
*****部門名称取得
     PERFORM  HJYOKEN-READ-SEC.
     IF       HJYOKEN-INV-FLG =  SPACE
              MOVE  JYO-F03  TO        BUMNM2
     END-IF.
*2009/09/30 部門CD追加 END   *************************
     MOVE     TOK-F02        TO        TNAME.
     MOVE     TOK-F04        TO        TKANA.
     MOVE     TOK-F03        TO        TRYAKU.
************* 99/09/27追加 ***************************
     MOVE     TOK-F801       TO        NYBN1.
     MOVE     TOK-F802       TO        NYBN2.
******************************************************
     MOVE     TOK-F05        TO        YUBIN.
     MOVE     TOK-F06        TO        JYU1.
     MOVE     TOK-F07        TO        JYU2.
     MOVE     TOK-F08        TO        TEL.
     MOVE     TOK-F09        TO        FAX.
     MOVE     TOK-F12(1)     TO        SIME1.
     MOVE     TOK-F12(2)     TO        SIME2.
     MOVE     TOK-F12(3)     TO        SIME3.
     MOVE     TOK-F84        TO        KUBUN1.
     MOVE     TOK-F88        TO        KUBUN2.
     MOVE     TOK-F91        TO        KUBUN3.
     MOVE     TOK-F92        TO        KUBUN4.
     MOVE     TOK-F93        TO        KUBUN5.
     MOVE     TOK-F94        TO        KUBUN6.
     MOVE     TOK-F89        TO        KUBUN7.
     MOVE     TOK-F82        TO        KUBUN8.
     MOVE     TOK-F55        TO        GENKA.
     MOVE     TOK-F56        TO        BAIKA.
     MOVE     TOK-F54        TO        UDENNO.
     MOVE     TOK-F57        TO        UKAINO.
     MOVE     TOK-F58        TO        UENDNO.
     MOVE     TOK-F59        TO        FDENNO.
     MOVE     TOK-F60        TO        FKAINO.
     MOVE     TOK-F61        TO        FENDNO.
************* 99/09/27追加 ***************************
     MOVE     TOK-F81        TO        SOKOCD  SOK-F01.
***  倉庫名称取得
     READ      ZSOKMS
       INVALID
               MOVE   SPACE       TO  SOKONM
       NOT INVALID
               MOVE  SOK-F02      TO  SOKONM
     END-READ.
******************************************************
************* 99/11/02追加 ***************************
     MOVE     TOK-F79        TO        DENCD.
************* 04/06/22追加 ***************************
     MOVE     TOK-F96        TO        BCHK.
     MOVE     TOK-F97        TO        ZEIKBN.
************* 09/08/19追加 ***************************
     MOVE     TOK-F78        TO        KANRI.
******************************************************
************* 11/04/14追加 ***************************
     MOVE     TOK-FIL1(2:1) TO        HCHSYU.
     MOVE     TOK-FIL1(3:1) TO        SYUONL.
     MOVE     TOK-FIL1(4:1) TO        SYUTEG.
************* 11/12/07追加 ***************************
     MOVE     TOK-F70(1:2)   TO        HARU1.
     MOVE     TOK-F70(3:2)   TO        HARU2.
     MOVE     TOK-F71(1:2)   TO        AKI1.
     MOVE     TOK-F71(3:2)   TO        AKI2.
*** 2012/10/31
     MOVE     TOK-F95        TO        STOKBN.
*
*2013/12/05↓ 消費税関連
*税扱い区分
     MOVE     TOK-F18        TO        ZEIA.
*税抜き計算区分
*2013/12/25↓
*    MOVE     TOK-F22            TO    WK-TOK-F22.
*    MOVE     WK-TOK-F22(9:1)    TO    ZEIN.
     MOVE     TOK-F20            TO    WK-TOK-F20.
     MOVE     WK-TOK-F20(11:1)   TO    ZEIN.
*2013/12/25↑
*2013/12/24↓
*計上税区分
*    MOVE     TOK-F19        TO        ZEIKB1.
*計上税区分名称
*    MOVE     50             TO   JYO-F01.
*    MOVE     ZEIKB1         TO   JYO-F02.
*    PERFORM  HJYOKEN-READ-SUB.
*    IF       JYO-INVALID-FLG  =    1
*             MOVE  SPACE    TO   ZEINM1
*    ELSE
*             MOVE  JYO-F03  TO   ZEINM1
*    END-IF.
*税区分改訂日
*    MOVE     TOK-F20        TO        CHGDAY.
*2013/12/25↓
*税抜計算区分改定日
     MOVE     TOK-F21        TO        CHGDAY.
*改定後税抜き計算区分
*2013/12/25↓
     MOVE     TOK-F22            TO    WK-TOK-F22.
     MOVE     WK-TOK-F22(9:1)    TO    ZEIN2.
*2013/12/25↑
*2013/12/25↑
*改訂後計上税区分
*    MOVE     TOK-F21        TO        ZEIKB2.
*改訂後計上税区分名称
*    MOVE     50             TO   JYO-F01.
*    MOVE     ZEIKB2         TO   JYO-F02.
*    PERFORM  HJYOKEN-READ-SUB.
*    IF       JYO-INVALID-FLG  =    1
*             MOVE  SPACE    TO   ZEINM2
*    ELSE
*             MOVE  JYO-F03  TO   ZEINM2
*    END-IF.
*2013/12/24↑
*2013/12/05↑ 消費税関連
*#2017/08/29 NAV ST
     MOVE     TOK-FIL1(1:1)  TO   MATOME.
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST
     MOVE     TOK-F90        TO   SEIKBN.
*#2019/09/22 NAV ED
******************************************************
 DSP-TEN1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TEN2-SEC           SECTION.
     MOVE     TORICD         TO        TOK-F01.
     MOVE     TOKUCD         TO        TOK-F52.
     MOVE     TNAME          TO        TOK-F02.
     MOVE     TKANA          TO        TOK-F04.
     MOVE     TRYAKU         TO        TOK-F03.
************* 99/09/27追加 ***************************
     MOVE     NYBN1          TO        TOK-F801.
     MOVE     NYBN2          TO        TOK-F802.
******************************************************
     MOVE     YUBIN          TO        TOK-F05.
     MOVE     JYU1           TO        TOK-F06.
     MOVE     JYU2           TO        TOK-F07.
     MOVE     TEL            TO        TOK-F08.
     MOVE     FAX            TO        TOK-F09.
     MOVE     SIME1          TO        TOK-F12(1).
     MOVE     SIME2          TO        TOK-F12(2).
     MOVE     SIME3          TO        TOK-F12(3).
     MOVE     KUBUN1         TO        TOK-F84.
     MOVE     KUBUN2         TO        TOK-F88.
     MOVE     KUBUN3         TO        TOK-F91.
     MOVE     KUBUN4         TO        TOK-F92.
     MOVE     KUBUN5         TO        TOK-F93.
     MOVE     KUBUN6         TO        TOK-F94.
     MOVE     KUBUN7         TO        TOK-F89.
     MOVE     KUBUN8         TO        TOK-F82.
     MOVE     GENKA          TO        TOK-F55.
     MOVE     BAIKA          TO        TOK-F56.
     MOVE     UDENNO         TO        TOK-F54.
     MOVE     UKAINO         TO        TOK-F57.
     MOVE     UENDNO         TO        TOK-F58.
     MOVE     FDENNO         TO        TOK-F59.
     MOVE     FKAINO         TO        TOK-F60.
     MOVE     FENDNO         TO        TOK-F61.
************* 99/09/27追加 ***************************
     MOVE     SOKOCD         TO        TOK-F81.
******************************************************
************* 99/11/02追加 ***************************
     MOVE     DENCD          TO        TOK-F79.
************* 04/06/22追加 ***************************
     MOVE     ZEIKBN         TO        TOK-F97.
     MOVE     BCHK           TO        TOK-F96.
************* 09/08/19追加 ***************************
     MOVE     KANRI          TO        TOK-F78.
******************************************************
************* 11/04/14追加 ***************************
     MOVE     HCHSYU         TO        TOK-FIL1(2:1).
     MOVE     SYUONL         TO        TOK-FIL1(3:1).
     MOVE     SYUTEG         TO        TOK-FIL1(4:1).
************* 11/12/07追加 ***************************
     MOVE     HARU1          TO        TOK-F70(1:2).
     MOVE     HARU2          TO        TOK-F70(3:2).
     MOVE     AKI1           TO        TOK-F71(1:2).
     MOVE     AKI2           TO        TOK-F71(3:2).
******************************************************
************* 09/09/30追加 ***************************
     MOVE     BUMON          TO        TOK-F75.
     MOVE     BUMHEN         TO        TOK-F76.
     MOVE     BUMON2         TO        TOK-F77.
*** 2012/10/31
     MOVE     STOKBN         TO        TOK-F95.
*2013/12/05↓ 消費税関連
*税扱い区分
     MOVE     ZEIA           TO        TOK-F18.
*税抜き計算区分
*2013/12/25↓
*    MOVE     ZEIN           TO        TOK-F22.
     MOVE     ZEIN           TO        TOK-F20.
*2013/12/25↑
*2013/12/24↓
*計上税区分
*    MOVE     ZEIKB1         TO        TOK-F19.
*税区分改訂日
*    MOVE     CHGDAY         TO        TOK-F20.
*2013/12/25↓
*税抜計算区分改定日
     MOVE     CHGDAY         TO        TOK-F21.
*改定後税抜き計算区分
*2013/12/25↓
     MOVE     ZEIN2          TO        TOK-F22.
*2013/12/25↑
*改訂後計上税区分
*    MOVE     ZEIKB2         TO        TOK-F21.
*2013/12/24↑
*2013/12/05↑ 消費税関連
******************************************************
*#2017/08/29 NAV ST 伝票纏め区分追加
     MOVE     MATOME         TO        TOK-FIL1(1:1).
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST 請求区分追加
     MOVE     SEIKBN         TO        TOK-F90.
*#2019/09/27 NAV ED
     MOVE     SYS-DATE       TO        TOK-F99.
 DSP-TEN2-EXIT.
     EXIT.
****************************************************************
*            マスタ更新履歴ファイル出力            *
****************************************************************
 MSTLOGF-WRITE-SEC           SECTION.
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
     ACCEPT    WK-TIME          FROM   TIME.
*
     MOVE     "01"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02.
     MOVE     PARA-TANCD          TO   MSL-F03.
     MOVE     SYORI               TO   MSL-F04.
     MOVE     DATE-AREA           TO   MSL-F05.
     MOVE     WK-TIME(1:6)        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F04
                                           MSL-F05
                                           MSL-F06
              INVALID
              MOVE     "01"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     SYORI         TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO  MSL-F07
              MOVE     DATA-TAIHI    TO  MSL-F08
*↓2019.03.15
              PERFORM  BEFORE-SEC
*↑2019.03.15
              WRITE    MSL-REC
              GO TO    MSTLOGF-WRITE-EXIT
     END-START.
     MOVE     0                   TO   SEQ.
*
 MSTLOG-WRITE-010.
     READ   MSTLOGF  NEXT  AT        END
            GO             TO        MSTLOGF-WRITE-EXIT
     END-READ.
*
     IF  (MSL-F01 > "01")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > SYORI)      OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "01"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     SYORI            TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
*↓2019.03.15
         PERFORM  BEFORE-SEC
*↑2019.03.15
         WRITE    MSL-REC
         GO       TO               MSTLOGF-WRITE-EXIT
     ELSE
         COMPUTE  SEQ =  MSL-F07  +  1
         GO           TO             MSTLOG-WRITE-010
     END-IF.
 MSTLOGF-WRITE-EXIT.
     EXIT.
****************************************************************
*  2019.03.15    修正前レコードセット
****************************************************************
 BEFORE-SEC                  SECTION.
*
     IF       SYORI   =    2
              MOVE  BEFORE-DATA   TO   MSL-F09
     END-IF.
*
 BEFORE-EXIT.
     EXIT.
****************************************************************
*            条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*
     MOVE    "22"      TO    JYO-F01.
*
     READ    HJYOKEN
             INVALID     MOVE "INV"     TO   HJYOKEN-INV-FLG
             NOT INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*2013/12/05↓
****************************************************************
*      3.0        条件ファイル索引                             *
****************************************************************
 HJYOKEN-READ-SUB       SECTION.
     MOVE     0                  TO   JYO-INVALID-FLG.
     READ     HJYOKEN
        INVALID
              MOVE      1        TO   JYO-INVALID-FLG
     END-READ.
 HJYOKEN-READ-END.
     EXIT.
*2013/12/05↑
****************************************************************
*            項目属性初期化
****************************************************************
 ZOKUSEI-CLR-SEC             SECTION.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TORICD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TORICD.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TOKUCD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TOKUCD.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TNAME.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TNAME.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TKANA.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TKANA.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TRYAKU.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TRYAKU.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  YUBIN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  YUBIN.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  NYBN1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  NYBN1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  NYBN2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  NYBN2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SOKOCD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SOKOCD.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SOKONM.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SOKONM.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  DENCD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  DENCD.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  JYU1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  JYU1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  JYU2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  JYU2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TEL.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TEL.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  FAX.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  FAX.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SIME1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SIME1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SIME2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SIME2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SIME3.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SIME3.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN3.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN3.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN4.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN4.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN5.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN5.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN6.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN6.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN7.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN7.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN8.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN8.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  FKAINO.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  FKAINO.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  FENDNO.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  FENDNO.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  FDENNO.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  FDENNO.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  UKAINO.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  UKAINO.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  UENDNO.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  UENDNO.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  GENKA.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  GENKA.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  BAIKA.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  BAIKA.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  R002.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  R002.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  ZEIKBN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIKBN.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  KANRI.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KANRI.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SYUONL.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUONL.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  SYUTEG.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUTEG.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  BUMON.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  BUMON.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  BUMON2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  BUMON2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  BUMHEN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  BUMHEN.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  HARU1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  HARU1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  HARU2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  HARU2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  AKI1.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  AKI1.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  AKI2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  AKI2.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  HCHSYU.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  HCHSYU.
***  2012/10/31
     MOVE     "M"        TO   EDIT-OPTION  OF  STOKBN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  STOKBN.
*
*2013/12/05↓
     MOVE     "M"        TO   EDIT-OPTION  OF  ZEIA.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIA.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  ZEIN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIN.
*
*2013/12/24↓
*    MOVE     "M"        TO   EDIT-OPTION  OF  ZEIKB1.
*    MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIKB1.
*
*    MOVE     "M"        TO   EDIT-OPTION  OF  ZEINM1.
*    MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEINM1.
*
*    MOVE     "M"        TO   EDIT-OPTION  OF  CHGDAY.
*    MOVE      SPACE     TO   EDIT-CURSOR  OF  CHGDAY.
*
*    MOVE     "M"        TO   EDIT-OPTION  OF  ZEIKB2.
*    MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIKB2.
*
*    MOVE     "M"        TO   EDIT-OPTION  OF  ZEINM2.
*    MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEINM2.
*2013/12/24↑
*2013/12/25↓
     MOVE     "M"        TO   EDIT-OPTION  OF  CHGDAY.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  CHGDAY.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  ZEIN2.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIN2.
*2013/12/25↑
*
*2013/12/05↑
*#2017/08/29 NAV ST
     MOVE     "M"        TO   EDIT-OPTION  OF  MATOME.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  MATOME.
*#2017/08/29 NAV ED
*#2019/09/27 NAV ST
     MOVE     "M"        TO   EDIT-OPTION  OF  SEIKBN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SEIKBN.
*#2019/09/27 NAV ED
*
 ZOKUSEI-CLR-EXIT.
     EXIT.
