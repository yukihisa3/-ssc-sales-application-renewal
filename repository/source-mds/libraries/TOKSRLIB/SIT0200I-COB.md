# SIT0200I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SIT0200I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　マスタ保守サブ                    *
*    業務名　　　　　　　：　取引先マスタ　　　　　　　        *
*    モジュール名　　　　：　取引先マスタ保守　                *
*    作成日／作成者　　　：　2018/03/12 NAV TAKAHASHI          *
*    処理内容　　　　　　：　取引先マスタの一般社員メンテ項目　*
*    　　　　　　　　　　　　の修正を行なう。　　　　　　　　　*
*    更新日／更新者　　　：　2019/03/15 NAV INOUE              *
*    更新内容　　　　　　：　S11300660                         *
*    　　　　　　　　　　　　マスタ更新履歴ファイル項目追加　　*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0200I.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             18/03/12.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          PG6000.
 OBJECT-COMPUTER.          PG6000.
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
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*画面ファイル
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
*条件ファイル
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
     COPY        FIT02001  OF       XMDLIB.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*画面制御情報
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
*メッセージ情報
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
     03  MSG29             PIC N(20) VALUE
                           NC"出場特定区分エラー".
     03  MSG30   PIC N(20) VALUE
                 NC"指定された値が違います".
     03  MSG31   PIC N(20) VALUE
                 NC"計上税区分が違います。".
     03  MSG32   PIC N(20) VALUE
                 NC"改訂日は未来日付を入力してください".
     03  MSG33   PIC N(20) VALUE
                 NC"日付が不正です。".
     03  MSG34   PIC N(20) VALUE
                 NC"改訂日を入力してください。".
     03  MSG35   PIC N(20) VALUE
                 NC"伝票纏め区分を正しく入力して下さい。".
*ＰＦガイダンス
     03  PMSG01            PIC N(20) VALUE
                           NC"_取消　_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_次検索".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消　_終了".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_終了　_再入力".
*システム日付変換
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
*その他ワーク項目
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  HJYOKEN-INV-FLG   PIC X(03) VALUE     SPACE.
     03  JYO-INVALID-FLG   PIC 9(01) VALUE     ZERO.
     03  WK-TOK-F22        PIC 9(09) VALUE     ZERO.
     03  WK-TOK-F20        PIC 9(11) VALUE     ZERO.
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
*ステータス情報
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
     03  MSL-ST            PIC X(02).
*ファイル異常時メッセージ
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
*マスタ更新ログＳＥＱ
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
     DISPLAY     FILE-ERR1 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HJYOKEN.
     DISPLAY     JYO-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ST    UPON      STA.
     DISPLAY     FILE-ERR4 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
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
 MAIN-020.  *>キー項目入力
*--< ＰＦキー選択 >--*
     MOVE        PMSG02    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           GO              TO        MAIN-SEC
       WHEN
        "F005"
           MOVE  9         TO        END-FLG
           GO              TO        MAIN-EXIT
       WHEN
        "F010"
           IF  (ZI-FLG = ZERO)
               IF  (INV-SW = 1)
                   MOVE      TORICD      TO        TOK-F01
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
           MOVE NC"次レコード無し"    TO        MSG1
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
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
       NOT INVALID
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE        SPACE     TO        MSG1.
     MOVE       "MSG1"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*得意先マスタ表示
     PERFORM     DSP-TEN1-SEC.
 MAIN-030.
     MOVE        PMSG03    TO        MSG2.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                             PERFORM ZOKUSEI-CLR-SEC
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU
                                             NYBN1   NYBN2
                                             JYU1    JYU2    TEL
                                             FAX
                                             HARU1 HARU2 AKI1 AKI2
                             GO        TO   MAIN-SEC
        WHEN       "F005"
           MOVE  9         TO        END-FLG
           GO              TO        MAIN-EXIT
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
                             PERFORM ZOKUSEI-CLR-SEC
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU
                                             NYBN1   NYBN2
                                             HARU1 HARU2 AKI1 AKI2
                             GO          TO   MAIN-020
         WHEN      "F009"
           MOVE     SPACE    TO        R002
           MOVE    "R002"    TO        DSP-GRP
           PERFORM  DSP-WR-SEC
           GO                TO        MAIN-030
        WHEN       "F005"
           MOVE  9         TO        END-FLG
           GO              TO        MAIN-EXIT
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
*画面から転送
     PERFORM   DSP-TEN2-SEC.
 MAIN-080.
     MOVE      TOK-REC        TO      DATA-TAIHI.
     REWRITE   TOK-REC.
**
     PERFORM  MSTLOGF-WRITE-SEC.
*
     PERFORM  ZOKUSEI-CLR-SEC.
     MOVE    "SCREEN"        TO      DSP-GRP.
     INITIALIZE      TORICD  TOKUCD  TNAME  TKANA   TRYAKU
                     NYBN1   NYBN2
                     JYU1    JYU2    TEL    FAX.
     PERFORM  DSP-WR-SEC.
     GO                      TO      MAIN-SEC.
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
     WRITE      FIT02001.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FIT02001.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FIT02001"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FIT02001.
     INITIALIZE              FIT02001.
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
     MOVE     TOK-F02        TO        TNAME.
     MOVE     TOK-F04        TO        TKANA.
     MOVE     TOK-F03        TO        TRYAKU.
     MOVE     TOK-F801       TO        NYBN1.
     MOVE     TOK-F802       TO        NYBN2.
     MOVE     TOK-F06        TO        JYU1.
     MOVE     TOK-F07        TO        JYU2.
     MOVE     TOK-F08        TO        TEL.
     MOVE     TOK-F09        TO        FAX.
     MOVE     TOK-F70(1:2)   TO        HARU1.
     MOVE     TOK-F70(3:2)   TO        HARU2.
     MOVE     TOK-F71(1:2)   TO        AKI1.
     MOVE     TOK-F71(3:2)   TO        AKI2.
 DSP-TEN1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TEN2-SEC           SECTION.
*    MOVE     TORICD         TO        TOK-F01.
*    MOVE     TOKUCD         TO        TOK-F52.
*    MOVE     TNAME          TO        TOK-F02.
*    MOVE     TKANA          TO        TOK-F04.
*    MOVE     TRYAKU         TO        TOK-F03.
*    MOVE     NYBN1          TO        TOK-F801.
*    MOVE     NYBN2          TO        TOK-F802.
*    MOVE     JYU1           TO        TOK-F06.
*    MOVE     JYU2           TO        TOK-F07.
*    MOVE     TEL            TO        TOK-F08.
*    MOVE     FAX            TO        TOK-F09.
*    MOVE     DENCD          TO        TOK-F79.
     MOVE     HARU1          TO        TOK-F70(1:2).
     MOVE     HARU2          TO        TOK-F70(3:2).
     MOVE     AKI1           TO        TOK-F71(1:2).
     MOVE     AKI2           TO        TOK-F71(3:2).
*    MOVE     HCHSYU         TO        TOK-FIL1(2:1).
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
     MOVE     "2"                 TO   MSL-F04.
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
              MOVE     "2"           TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO  MSL-F07
              MOVE     DATA-TAIHI    TO  MSL-F08
*↓2019.03.15
              MOVE     BEFORE-DATA   TO  MSL-F09
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
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > 2    )      OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "01"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     2                TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
*↓2019.03.15
         MOVE     BEFORE-DATA      TO  MSL-F09
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
*            項目属性初期化
****************************************************************
 ZOKUSEI-CLR-SEC             SECTION.
*
     MOVE     "M"        TO   EDIT-OPTION  OF  TORICD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  TORICD.
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
 ZOKUSEI-CLR-EXIT.
     EXIT.

```
