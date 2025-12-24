****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　マスタメンテ                          *
*    モジュール名　　：　ルート条件マスタメンテ                *
*                        （ＩＴ統制対応）SSY9901I改修
*    作成日／更新日　：　09/03/24                              *
*    作成者／更新者　：　NAV･ASSIST                            *
*    更新日／更新者　：                                        *
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0130I.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             09/03/24.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-150SI.
 OBJECT-COMPUTER.          K-150SI.
 SPECIAL-NAMES.
     STATION     IS        STA.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*ルート条件マスタ
     SELECT      JHMRUTF   ASSIGN    TO        DA-01-VI-JHMRUTL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       JRU-F01
                                               JRU-F02
                                               JRU-F03
                           FILE      STATUS    JRU-ST.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT      ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        DA-01-VI-MSTLOGL1
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
*ルート条件マスタ
 FD  JHMRUTF
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMRUTF   OF        XFDLIB
     JOINING     JRU       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FIT01301    OF      XMDLIB.
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
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"Ｙで入力して下さい".
     03  MSG07             PIC N(20) VALUE
                           NC"取引先ＣＤを入力して下さい".
     03  MSG09             PIC N(20) VALUE
                           NC"取引先Ｍが登録されていません".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG11             PIC N(20) VALUE
                           NC"振分倉庫コードが未入力です".
     03  MSG12             PIC N(20) VALUE
                           NC"倉庫Ｍが登録されていません".
*
     03  PMSG01            PIC N(20) VALUE
                           NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_次検索".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_再入力".
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
****  システム日付  ****
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
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
     03  WK-BUNRUI         PIC X(04) VALUE     SPACE.
     03  WK-RUTO           PIC X(08) VALUE     SPACE.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-FSOKO          PIC X(02) VALUE     SPACE.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  JRU-ST            PIC X(02).
     03  JRU-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
*
     03  FILE-ERR1         PIC N(12) VALUE
                                  NC"ルート条件マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"取引先マスタ異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
     03  FILE-ERR4         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR5         PIC N(12) VALUE
                                  NC"マスタ更新履歴Ｆ異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(100).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
******************************************************************
 PROCEDURE                 DIVISION  USING PARA-BUMONCD
                                           PARA-TANCD
                                           PARA-UPDTDATE
                                           PARA-UPDTIME.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 JRU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           JHMRUTF.
     DISPLAY     JRU-ST    UPON      STA.
     DISPLAY     FILE-ERR1 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTOKMS.
     DISPLAY     TOK-ST    UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           MSTLOGF.
     DISPLAY     MSL-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
     DISPLAY     FILE-ERR4 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL     END-FLG   =    9.
     PERFORM  END-SEC.
     STOP     RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HTOKMS    ZSOKMS
              I-O       JHMRUTF   MSTLOGF
                        DSPFILE.
     MOVE     ZERO      TO        END-FLG.
     MOVE     SPACE     TO        JRU-REC.
     INITIALIZE                   JRU-REC.
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
*受渡しパラメタセット
     MOVE      DATE-AREA          TO   PARA-UPDTDATE.
     MOVE      WK-TIME(1:6)       TO   PARA-UPDTIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-INIT-SEC.
*処理区分入力
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
         GO                TO        MAIN-010
     END-IF.
 MAIN-020.
*--< ＰＦキー選択 >--*
     IF  (SYORI  =  2)
          MOVE   PMSG02    TO        MSG2
     ELSE
          MOVE   PMSG03    TO        MSG2
     END-IF.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*    取引先，分類，ルートキー入力
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
                   MOVE      TORICD    TO        JRU-F01
                   MOVE      BUNRUI    TO        JRU-F02
                   MOVE      RUTO      TO        JRU-F03
                   START     JHMRUTF  KEY IS  >  JRU-F01  JRU-F02
                                                 JRU-F03
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  JHMRUTF    NEXT
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
                MOVE NC"次レコード無し" TO     MSG1
           ELSE
                MOVE    MSG10        TO        MSG1
           END-IF
           MOVE   "MSG1"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO              TO        MAIN-020
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
*取引先コード CHK
     IF  (TORICD  =  ZERO)
         MOVE    MSG07     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TORINM
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
     END-IF.
*
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE    MSG09     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TORINM
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
       NOT INVALID
         MOVE    TOK-F02   TO        TORINM
         MOVE    SPACE     TO        MSG1
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
     END-READ.
*    ルート条件マスタチェック
     MOVE        SPACE     TO        JRU-REC.
     INITIALIZE                      JRU-REC.
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        TORICD    TO        JRU-F01.
     MOVE        BUNRUI    TO        JRU-F02.
     MOVE        RUTO      TO        JRU-F03.
     READ  JHMRUTF
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  BUNRUI
             MOVE    SPACE     TO        EDIT-CURSOR  OF  BUNRUI
             MOVE   "M"        TO        EDIT-OPTION  OF  RUTO
             MOVE    SPACE     TO        EDIT-CURSOR  OF  RUTO
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  BUNRUI
             MOVE   "R"        TO        EDIT-OPTION  OF  RUTO
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  BUNRUI
             MOVE   "R"        TO        EDIT-OPTION  OF  RUTO
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  BUNRUI
             MOVE    SPACE     TO        EDIT-CURSOR  OF  BUNRUI
             MOVE   "M"        TO        EDIT-OPTION  OF  RUTO
             MOVE    SPACE     TO        EDIT-CURSOR  OF  RUTO
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "M"        TO        EDIT-OPTION  OF  BUNRUI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  BUNRUI.
     MOVE       "M"        TO        EDIT-OPTION  OF  RUTO.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  RUTO.
     MOVE        SPACE     TO        MSG1.
*ルート条件マスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TER1-SEC
     END-IF.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     IF  (SYORI  =  3)
         GO                TO        MAIN-040
     END-IF.
 MAIN-030.
     MOVE        PMSG03    TO        MSG2.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE   SPACE   TO  BDY1      KAKU
                    MOVE  "SCREEN" TO  DSP-GRP
                    PERFORM DSP-WR-SEC
                    INITIALIZE     TORICD  TORINM  BUNRUI
                                   RUTO    RNAME
                                   FSOKO   FNAME
                    GO             TO  MAIN-020
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
     MOVE     SPACE          TO   MSG1.
*    振分倉庫コード
     IF  FSOKO    =     SPACE
         MOVE     "R"        TO   EDIT-OPTION  OF  FSOKO
         MOVE     "C"        TO   EDIT-CURSOR  OF  FSOKO
         MOVE      MSG11     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  FSOKO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  FSOKO
     END-IF.
*    倉庫マスタチェック
     MOVE     FSOKO          TO        SOK-F01.
     READ     ZSOKMS         INVALID
              MOVE MSG12     TO        MSG1
              MOVE "MSG1"    TO        DSP-GRP
              MOVE SPACE     TO        FNAME
              PERFORM DSP-WR-SEC
              MOVE "R"       TO        EDIT-OPTION  OF  FSOKO
              MOVE "C"       TO        EDIT-CURSOR  OF  FSOKO
              MOVE "GRP001"  TO        DSP-GRP
              PERFORM DSP-WR-SEC
              GO             TO        MAIN-030
     NOT  INVALID
              MOVE SOK-F02   TO        FNAME
              MOVE SPACE     TO        MSG1
              MOVE "M"       TO        EDIT-OPTION  OF  FSOKO
              MOVE SPACE     TO        EDIT-CURSOR  OF  FSOKO
     END-READ.
     IF       RNAME    =     SPACE
              MOVE     FNAME      TO   RNAME
     END-IF.
*
     MOVE     "Y"            TO   R002.
     MOVE     "SCREEN"       TO   DSP-GRP.
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
                             MOVE   SPACE   TO   BDY1  KAKU
                             MOVE  "SCREEN" TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE     TORICD  TORINM  BUNRUI
                                            RUTO    RNAME
                                            FSOKO   FNAME
                             GO             TO   MAIN-020
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
           MOVE    MSG06     TO   MSG1
           MOVE   "MSG1"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE     TO   MSG1
           MOVE   "MSG1"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
     END-IF.
*
     IF  (SYORI  =  3)
           GO          TO   MAIN-080.
*画面から転送
     PERFORM  DSP-TER2-SEC.
 MAIN-080.
     EVALUATE   SYORI
         WHEN     1
              MOVE     JRU-REC    TO  DATA-TAIHI
              WRITE    JRU-REC
              END-WRITE
         WHEN     2
              MOVE      JRU-REC   TO  DATA-TAIHI
              REWRITE   JRU-REC
              END-REWRITE
         WHEN     3
              MOVE      JRU-REC   TO  DATA-TAIHI
              DELETE    JHMRUTF
              END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
     MOVE    SPACE          TO      BDY1    KAKU.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
     INITIALIZE     TORICD  TORINM  BUNRUI  RUTO  RNAME
                    FSOKO   FNAME.
     GO             TO      MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HTOKMS     ZSOKMS  MSTLOGF
                       JHMRUTF    DSPFILE.
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
     MOVE       SPACE        TO  DSP-PRO.
     MOVE       HEN-DATE     TO  SDATE.
     MOVE       HEN-TIME     TO  STIME.
     WRITE      FIT01301.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FIT01301.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FIT01301"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FIT01301.
     INITIALIZE              FIT01301.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*    ルート条件マスタのレコードを画面に転送                  *
****************************************************************
 DSP-TER1-SEC              SECTION.
     MOVE     JRU-F01        TO        TORICD   WK-TORICD.
     PERFORM  TOK-RD-SEC.
*
     MOVE     JRU-F02        TO        BUNRUI   WK-BUNRUI.
*
     MOVE     JRU-F03        TO        RUTO     WK-RUTO.
*
     MOVE     JRU-F04        TO        RNAME.
*
     MOVE     JRU-F05        TO        FSOKO    WK-FSOKO.
     PERFORM  SOK-RD-SEC.
*
 DSP-TER1-EXIT.
     EXIT.
****************************************************************
*            画面よりＭレコードに転送            *
****************************************************************
 DSP-TER2-SEC           SECTION.
     MOVE     TORICD         TO        JRU-F01.
     MOVE     BUNRUI         TO        JRU-F02.
     MOVE     RUTO           TO        JRU-F03.
     MOVE     RNAME          TO        JRU-F04.
     MOVE     FSOKO          TO        JRU-F05.
*
 DSP-TER2-EXIT.
     EXIT.
****************************************************************
*  取引先マスタより取引先名取得                                *
****************************************************************
 TOK-RD-SEC             SECTION.
*
     MOVE     WK-TORICD      TO        TOK-F01.
     READ     HTOKMS         INVALID
              MOVE ALL NC"＊" TO       TORINM
              MOVE 1         TO        ERR-FLG
     NOT  INVALID
              MOVE TOK-F02   TO        TORINM
              MOVE ZERO      TO        ERR-FLG
     END-READ.
*
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*  倉庫マスタより倉庫名取得                                    *
****************************************************************
 SOK-RD-SEC             SECTION.
     MOVE     WK-FSOKO       TO        SOK-F01.
     READ     ZSOKMS         INVALID
              MOVE ALL NC"＊"    TO    FNAME
              MOVE 1             TO    ERR-FLG
     NOT  INVALID
              MOVE SOK-F02       TO    FNAME
              MOVE ZERO          TO    ERR-FLG
     END-READ.
 SOK-RD-EXIT.
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
     MOVE     "06"                TO   MSL-F01.
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
              MOVE     "06"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     SYORI         TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO    MSL-F07
              MOVE     DATA-TAIHI    TO    MSL-F08
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
     IF  (MSL-F01 > "06")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > SYORI)      OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "06"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     SYORI            TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
         WRITE    MSL-REC
         GO       TO               MSTLOGF-WRITE-EXIT
     ELSE
         COMPUTE  SEQ =  MSL-F07  +  1
         GO       TO               MSTLOG-WRITE-010
     END-IF.
 MSTLOGF-WRITE-EXIT.
     EXIT.
