***************************************************************
*
*   商品名称マスタ保守１（ＩＴ統制）
*   SIT0095I(SMNT030改修)
*   更新履歴 ：
*     2011/10/14 飯田/NAV 基幹サーバ統合・業務改善
*     2012/10/09 高橋/NAV ＬＩＮＫＳ連携　物流束区分追加
*     2016/12/21 井上/NAV ＡＣＯＳ・社内　振替商品区分追加
*     2017/05/23 高橋/NAV 廃盤区分　２：出荷停止追加　　　　
*     2019/03/15 井上/NAV S11300660
*                         マスタ更新履歴ファイル項目追加
*     2020/07/09 高橋/NAV ストックＮＯ管理区分追加　　　　　
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0095I.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/19.
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
*商品マスタ*
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           FILE      STATUS    MEI-ST.
     SELECT      ZSHIMS1   ASSIGN    TO        DA-01-VI-ZSHIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       SHI-F01.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
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
*商品在庫マスタ
 FD  HMEIMS
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
 FD  ZSHIMS1.
     COPY        ZSHIMS    OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
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
 01  DSP-REC            PIC  X(2000).
     COPY     FIT00901  OF   XMDLIB.
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
                           NC"名称変更区分エラー".
     03  MSG04             PIC N(20) VALUE
                           NC"商品コードを入力してください".
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"Ｙで入力して下さい".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG11             PIC N(20) VALUE
                           NC"仕入先が未登録です".
     03  MSG12             PIC N(20) VALUE
                           NC"定番区分に誤りがあります".
     03  MSG13             PIC N(20) VALUE
                           NC"季節区分に誤りがあります".
     03  MSG14             PIC N(20) VALUE
                           NC"自動発注区分に誤りがあります".
     03  MSG15             PIC N(20) VALUE
                           NC"廃盤区分に誤りがあります".
     03  MSG16             PIC N(20) VALUE
                           NC"分類区分に誤りがあります".
     03  MSG17             PIC N(20) VALUE
                           NC"管理区分に誤りがあります".
* 2011/10/14,S  S.I/NAV
     03  MSG18             PIC N(20) VALUE
                           NC"サカタ２０分類に誤りがあります".
     03  MSG19             PIC N(20) VALUE
                           NC"小売連携区分に誤りがあります".
* 2011/10/14,E  S.I/NAV
* 2012/10/09,S  T.T/NAV
     03  MSG20             PIC N(20) VALUE
                           NC"物流束区分に誤りがあります".
* 2012/10/09,E  T.T/NAV
*
*↓2016/12/21
     03  MSG21             PIC N(20) VALUE
                           NC"振替商品区分に誤りがあります".
*↑2016/12/21
*↓2020/07/09
     03  MSG22             PIC N(20) VALUE
              NC"ストックＮＯ管理区分に誤りがあります".
*↑2020/07/09
*
     03  PMSG01            PIC N(20) VALUE
                           NC"_取消　_次検索".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消　_再入力".
     03  PMSG04            PIC N(20) VALUE
                           NC"_終了".
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
*
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
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  MSL-ST            PIC X(02).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"商品マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"条件ファイル異常！".
     03  FILE-ERR4         PIC N(12) VALUE
                                  NC"マスタ更新履歴Ｆ異常！".
 01  IXD                   PIC 9(06) VALUE ZERO.
 01  IXB                   PIC 9(06) VALUE ZERO.
 01  IXC                   PIC 9(06) VALUE ZERO.
* 2011/10/14,S  S.I/NAV
 01  FG-HJYOKEN-INV        PIC  9(01).
* 2011/10/14,E  S.I/NAV
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
*↓2019.03.15
 01  BEFORE-DATA           PIC  X(170).
*↑2019.03.15
 01  DATA-TAIHI            PIC  X(170).
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
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HMEIMS.
     DISPLAY     MEI-ST    UPON      STA.
**** DISPLAY     MEI-ST1   UPON      STA.
     DISPLAY     FILE-ERR1 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HJYOKEN.
     DISPLAY     JYO-ST    UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ST    UPON      STA.
     DISPLAY     FILE-ERR4 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
**** DISPLAY     DSP-ST1   UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
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
     OPEN        I-O       HMEIMS  MSTLOGF
                           DSPFILE.
     OPEN        INPUT     ZSHIMS1
                           HJYOKEN.
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
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE  MEI-REC.
*
*    ACCEPT     WSYS-YMD   FROM      DATE.
*    IF   (WSYS-YY  <  89)
*          MOVE     20     TO        WSYS-Y1
*      ELSE
*          MOVE     19     TO        WSYS-Y1
*    END-IF.
*    MOVE        WSYS-DATE TO        SYS-DATE.
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
*処理区分入力
 MAIN-010.
     MOVE        PMSG04    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SYORI"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE     9      TO        END-FLG
           GO              TO        MAIN-EXIT
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG10   TO        MSG1
           MOVE   "MSG1"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO              TO        MAIN-010
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
     IF  (SYORI  =  2 OR 3)
         MOVE    PMSG01    TO        MSG2
     ELSE
         MOVE    PMSG02    TO        MSG2
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
                   MOVE        SHOCD     TO        MEI-F011
                   MOVE        TAN1      TO        MEI-F0121
                   MOVE        TAN2      TO        MEI-F0122
                   MOVE        TAN3      TO        MEI-F0123
                   START     HMEIMS  KEY IS  >
                        MEI-F011  MEI-F0121  MEI-F0122 MEI-F0123
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  HMEIMS    NEXT
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
               MOVE    MSG10   TO    MSG1
           END-IF
           MOVE    "MSG1"      TO    DSP-GRP
           PERFORM  DSP-WR-SEC
           GO                  TO    MAIN-020
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
* ｼｮｳﾋﾝ ｺｰﾄﾞ CHK
 MAIN-001.
*商品ＣＤ右詰め処理
*
       IF      SHOCD          NOT =     SPACE
            PERFORM VARYING IXB    FROM  1   BY   1
                    UNTIL   IXB    >     7
                PERFORM VARYING IXC    FROM  8   BY  -1
                        UNTIL   IXC    <     2
                  IF  SHOCD      (IXC:1)   =  SPACE
                      COMPUTE IXD    =     IXC   -   1
                      MOVE  SHOCD      (IXD:1)   TO
                                                SHOCD      (IXC:1)
                      MOVE  SPACE                TO
                                                SHOCD      (IXD:1)
                  END-IF
              END-PERFORM
            END-PERFORM
**
            PERFORM     VARYING    IXB   FROM    1   BY   1
                        UNTIL      (IXB   >      7 ) OR
                        (SHOCD     (IXB:1) NOT =  SPACE)
              IF        SHOCD     (IXB:1)        =   SPACE
                        MOVE       "0"   TO      SHOCD     (IXB:1)
              END-IF
            END-PERFORM
       END-IF
*
       IF      TAN1           NOT =     SPACE
            PERFORM VARYING IXB    FROM  1   BY   1
                    UNTIL   IXB    >     4
                PERFORM VARYING IXC    FROM  5   BY  -1
                        UNTIL   IXC    <     2
                  IF  TAN1       (IXC:1)   =  SPACE
                      COMPUTE IXD    =     IXC   -   1
                      MOVE  TAN1       (IXD:1)   TO
                                                TAN1       (IXC:1)
                      MOVE  SPACE                TO
                                                TAN1       (IXD:1)
                  END-IF
              END-PERFORM
            END-PERFORM
**
            PERFORM     VARYING    IXB   FROM    1   BY   1
                        UNTIL      (IXB   >      4 ) OR
                        (TAN1      (IXB:1) NOT =  SPACE)
              IF        TAN1      (IXB:1)        =   SPACE
                        MOVE       SPACE TO      TAN1      (IXB:1)
              END-IF
            END-PERFORM
       END-IF
*93.11.17
     IF  (SHOCD =  SPACE) AND (TAN1  =  SPACE)
     AND (TAN2  =  SPACE)  AND (TAN3  =  SPACE)
         MOVE   "R"        TO        EDIT-OPTION  OF  SHOCD
         MOVE   "C"        TO        EDIT-CURSOR  OF  SHOCD
         MOVE   "R"        TO        EDIT-OPTION  OF  TAN1
         MOVE   "C"        TO        EDIT-CURSOR  OF  TAN1
         MOVE   "R"        TO        EDIT-OPTION  OF  TAN2
         MOVE   "C"        TO        EDIT-CURSOR  OF  TAN2
         MOVE   "R"        TO        EDIT-OPTION  OF  TAN3
         MOVE   "C"        TO        EDIT-CURSOR  OF  TAN3
         MOVE    MSG04     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  SHOCD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  SHOCD
         MOVE   "M"        TO        EDIT-OPTION  OF  TAN1
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN1
         MOVE   "M"        TO        EDIT-OPTION  OF  TAN2
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN2
         MOVE   "M"        TO        EDIT-OPTION  OF  TAN3
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN3
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC.
*
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        SHOCD     TO        MEI-F011.
     MOVE        TAN1      TO        MEI-F0121.
     MOVE        TAN2      TO        MEI-F0122.
     MOVE        TAN3      TO        MEI-F0123.
     READ  HMEIMS
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "M"        TO        EDIT-OPTION  OF  SHOCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SHOCD
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN1
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN1
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN2
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN2
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN3
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN3
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  SHOCD
             MOVE   "C"        TO        EDIT-CURSOR  OF  SHOCD
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN1
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN1
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN2
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN2
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN3
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN3
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  SHOCD
             MOVE   "C"        TO        EDIT-CURSOR  OF  SHOCD
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN1
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN1
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN2
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN2
             MOVE   "R"        TO        EDIT-OPTION  OF  TAN3
             MOVE   "C"        TO        EDIT-CURSOR  OF  TAN3
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  SHOCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SHOCD
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN1
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN1
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN2
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN2
             MOVE   "M"        TO        EDIT-OPTION  OF  TAN3
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN3
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  SHOCD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SHOCD.
     MOVE       "M"        TO        EDIT-OPTION  OF  TAN1.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TAN1.
     MOVE       "M"        TO        EDIT-OPTION  OF  TAN2.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TAN2.
     MOVE       "M"        TO        EDIT-OPTION  OF  TAN3.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TAN3.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE        SPACE     TO        MSG1.
     MOVE       "MSG1"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*商品マスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TEN1-SEC
         MOVE       "GRPKEY"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC
         MOVE       "GRP001"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC.
     IF  (SYORI  =  3)
         GO                TO        MAIN-040.
 MAIN-030.
     MOVE        PMSG02    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                             MOVE   SPACE   TO  MEISAI  R002
                             MOVE  "SCREEN" TO  DSP-GRP
                             PERFORM   DSP-WR-SEC
                             INITIALIZE     SHOCD  TAN1   TAN2
                                            TAN3   SHON1  SHON2
                                            KANA1  KANA2  GENKA
                                            GENTAN URITAN KUBUN1
                                            KUBUN2 SIRCD  JANCD
                                            IRESU  SIRNM  HATKBN
                                            TEIBAN KISETU KINAME
                                            R002   HAIBAN
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
***  MOVE     "MSG1"         TO   DSP-GRP.
***  PERFORM  DSP-WR-SEC.
***  MOVE       "GRP001"   TO        DSP-GRP.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM  DSP-WR-SEC.
*名称変更区分
     IF  (KUBUN1  =  0 OR 1)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN1
***      MOVE     "GRP001"   TO   DSP-GRP
***      PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN1
         MOVE      MSG03     TO   MSG1
***      MOVE     "MSG1"     TO   DSP-GRP
***      PERFORM   DSP-WR-SEC
***      MOVE     "GRP001"   TO   DSP-GRP
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*
*93.11.19
*仕入先コード
     MOVE     "M"        TO   EDIT-OPTION  OF  SIRCD.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  SIRCD.
     MOVE     SIRCD          TO   SHI-F01.
     READ     ZSHIMS1
         INVALID
             MOVE     "R"        TO   EDIT-OPTION  OF  SIRCD
             MOVE     "C"        TO   EDIT-CURSOR  OF  SIRCD
             MOVE      MSG11     TO   MSG1
***          MOVE     "MSG1"     TO   DSP-GRP
***          PERFORM   DSP-WR-SEC
***          MOVE     "GRP001"   TO   DSP-GRP
             MOVE     "SCREEN"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         NOT INVALID
             MOVE      SHI-F02   TO   SIRNM
***          MOVE     "GRP001"   TO   DSP-GRP
***          PERFORM   DSP-WR-SEC
     END-READ.
*
*自動発注区分
     IF  (HATKBN  =  0 OR 1)
         MOVE     "M"        TO   EDIT-OPTION  OF  HATKBN
         MOVE      SPACE     TO   EDIT-CURSOR  OF  HATKBN
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  HATKBN
         MOVE     "C"        TO   EDIT-CURSOR  OF  HATKBN
         MOVE      MSG14     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*
*定番区分
     IF  (TEIBAN  =  0 OR 1)
         MOVE     "M"        TO   EDIT-OPTION  OF  TEIBAN
         MOVE      SPACE     TO   EDIT-CURSOR  OF  TEIBAN
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  TEIBAN
         MOVE     "C"        TO   EDIT-CURSOR  OF  TEIBAN
         MOVE      MSG12     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*
*季節区分
     IF  KISETU    NOT =     SPACE
         MOVE      81             TO   JYO-F01
         MOVE      KISETU         TO   JYO-F02
         READ      HJYOKEN
              INVALID
                   MOVE      SPACE     TO   KINAME
                   MOVE  "R"  TO   EDIT-OPTION  OF  KISETU
                   MOVE  "C"  TO   EDIT-CURSOR  OF  KISETU
                   MOVE   MSG13     TO   MSG1
                   MOVE  "SCREEN"   TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   GO   TO   MAIN-030
              NOT INVALID
                   MOVE      JYO-F14   TO   KINAME
                   MOVE "M"       TO   EDIT-OPTION  OF  KISETU
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  KISETU
         END-READ
       ELSE
         MOVE      SPACE     TO   KINAME
         MOVE     "R"        TO   EDIT-OPTION  OF  KISETU
         MOVE     "C"        TO   EDIT-CURSOR  OF  KISETU
         MOVE      MSG13     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*
*廃盤区分
*2017/05/23 NAV ST ２：出荷停止追加（パラ：区分＝１の時有効）
     IF  HAIBAN    NOT =     SPACE
*********IF   HAIBAN  NOT =  "1"
*             MOVE  "R"  TO   EDIT-OPTION  OF  HAIBAN
*             MOVE  "C"  TO   EDIT-CURSOR  OF  HAIBAN
*             MOVE   MSG15     TO   MSG1
*             MOVE  "SCREEN"   TO   DSP-GRP
*             PERFORM   DSP-WR-SEC
*             GO   TO   MAIN-030
*        ELSE
*             MOVE "M"   TO   EDIT-OPTION  OF  HAIBAN
*             MOVE SPACE TO   EDIT-CURSOR  OF  HAIBAN
*********END-IF
         IF   HAIBAN  =  "1"  OR  "2"
              MOVE "M"   TO   EDIT-OPTION  OF  HAIBAN
              MOVE SPACE TO   EDIT-CURSOR  OF  HAIBAN
         ELSE
              MOVE  "R"  TO   EDIT-OPTION  OF  HAIBAN
              MOVE  "C"  TO   EDIT-CURSOR  OF  HAIBAN
              MOVE   MSG15     TO   MSG1
              MOVE  "SCREEN"   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              GO   TO   MAIN-030
         END-IF
     ELSE
         MOVE "M"   TO   EDIT-OPTION  OF  HAIBAN
         MOVE SPACE TO   EDIT-CURSOR  OF  HAIBAN
     END-IF.
*
     MOVE     "Y"            TO   R002.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
***  MOVE     "R002"         TO   DSP-GRP.
***  PERFORM   DSP-WR-SEC.
*
****09/09/01      分類・管理区分追加
*分類区分
     IF  BUNRUI    NOT =     SPACE
         MOVE      91             TO   JYO-F01
         MOVE      BUNRUI         TO   JYO-F02
         READ      HJYOKEN
              INVALID
                   MOVE       SPACE     TO   BUNRUI
                   MOVE       SPACE     TO   BUNMEI
                   MOVE  "R"  TO   EDIT-OPTION  OF  BUNRUI
                   MOVE  "C"  TO   EDIT-CURSOR  OF  BUNRUI
                   MOVE   MSG16     TO   MSG1
                   MOVE  "SCREEN"   TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   GO   TO   MAIN-030
              NOT INVALID
                   MOVE      JYO-F03   TO   BUNMEI
                   MOVE "M"       TO   EDIT-OPTION  OF  BUNRUI
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  BUNRUI
         END-READ
       ELSE
         MOVE      SPACE     TO   BUNRUI
         MOVE      SPACE     TO   BUNMEI
         MOVE     "R"        TO   EDIT-OPTION  OF  BUNRUI
         MOVE     "C"        TO   EDIT-CURSOR  OF  BUNRUI
         MOVE      MSG16     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.

*管理区分
     IF  KANRI     NOT =     SPACE
         IF   KANRI   NOT =  "1"
              MOVE  "R"  TO   EDIT-OPTION  OF  KANRI
              MOVE  "C"  TO   EDIT-CURSOR  OF  KANRI
              MOVE   MSG17     TO   MSG1
              MOVE  "SCREEN"   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              GO   TO   MAIN-030
         ELSE
              MOVE "M"   TO   EDIT-OPTION  OF  KANRI
              MOVE SPACE TO   EDIT-CURSOR  OF  KANRI
         END-IF
     ELSE
         MOVE "M"   TO   EDIT-OPTION  OF  KANRI
         MOVE SPACE TO   EDIT-CURSOR  OF  KANRI
     END-IF.

* 2011/10/14,S  S.I/NAV
*サカタ２０分類
     MOVE  10               TO  JYO-F01.
     MOVE  BNR20            TO  JYO-F02.
     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-INV = 1
         MOVE  SPACE        TO  BNR20N
         MOVE  "R"          TO  EDIT-OPTION OF BNR20
         MOVE  "C"          TO  EDIT-CURSOR OF BNR20
         MOVE   MSG18       TO  MSG1
         MOVE  "SCREEN"     TO  DSP-GRP
         PERFORM  DSP-WR-SEC
         GO TO  MAIN-030
     ELSE
         MOVE JYO-F03       TO  BNR20N
         MOVE "M"           TO  EDIT-OPTION OF BNR20
         MOVE  SPACE        TO  EDIT-CURSOR OF BNR20
     END-IF.
* 2011/10/14,E  S.I/NAV
* 2011/10/14,S  S.I/NAV
*小売区分
     IF  KOURI NOT = SPACE
         IF KOURI NOT = "1"
            MOVE  "R"       TO  EDIT-OPTION OF KOURI
            MOVE  "C"       TO  EDIT-CURSOR OF KOURI
            MOVE  MSG19     TO  MSG1
            MOVE  "SCREEN"  TO  DSP-GRP
            PERFORM  DSP-WR-SEC
            GO TO  MAIN-030
         ELSE
            MOVE "M"        TO  EDIT-OPTION OF KOURI
            MOVE SPACE      TO  EDIT-CURSOR OF KOURI
         END-IF
     ELSE
         MOVE "M"           TO  EDIT-OPTION OF KOURI
         MOVE SPACE         TO  EDIT-CURSOR OF KOURI
     END-IF.
* 2011/10/14,E  S.I/NAV
* 2012/10/09,S  T.T/NAV
*物流束区分
     IF  BUTUR NOT = SPACE
         IF BUTUR NOT = "1"
            MOVE  "R"       TO  EDIT-OPTION OF BUTUR
            MOVE  "C"       TO  EDIT-CURSOR OF BUTUR
            MOVE  MSG20     TO  MSG1
            MOVE  "SCREEN"  TO  DSP-GRP
            PERFORM  DSP-WR-SEC
            GO TO  MAIN-030
         ELSE
            MOVE "M"        TO  EDIT-OPTION OF BUTUR
            MOVE SPACE      TO  EDIT-CURSOR OF BUTUR
         END-IF
     ELSE
         MOVE "M"           TO  EDIT-OPTION OF BUTUR
         MOVE SPACE         TO  EDIT-CURSOR OF BUTUR
     END-IF.
* 2012/10/09,E  T.T/NAV
*
*↓2016/12/21
*　振替商品区分
     IF  FURIKA NOT = SPACE
         IF FURIKA NOT = "1"
            MOVE  "R"       TO  EDIT-OPTION OF FURIKA
            MOVE  "C"       TO  EDIT-CURSOR OF FURIKA
            MOVE  MSG21     TO  MSG1
            MOVE  "SCREEN"  TO  DSP-GRP
            PERFORM  DSP-WR-SEC
            GO TO  MAIN-030
         ELSE
            MOVE "M"        TO  EDIT-OPTION OF FURIKA
            MOVE SPACE      TO  EDIT-CURSOR OF FURIKA
         END-IF
     ELSE
         MOVE "M"           TO  EDIT-OPTION OF FURIKA
         MOVE SPACE         TO  EDIT-CURSOR OF FURIKA
     END-IF.
*↑2016/12/21
*↓2020/07/09 ストックＮＯ管理区分追加
*　ストックＮＯ管理区分
     IF  STNO NOT NUMERIC
         MOVE     ZERO      TO STNO
     END-IF.
     IF  STNO NOT = ZERO
         IF STNO NOT = 1
            MOVE  "R"       TO  EDIT-OPTION OF STNO
            MOVE  "C"       TO  EDIT-CURSOR OF STNO
            MOVE  MSG22     TO  MSG1
            MOVE  "SCREEN"  TO  DSP-GRP
            PERFORM  DSP-WR-SEC
            GO TO  MAIN-030
         ELSE
            MOVE "M"        TO  EDIT-OPTION OF STNO
            MOVE SPACE      TO  EDIT-CURSOR OF STNO
         END-IF
     ELSE
         MOVE "M"           TO  EDIT-OPTION OF STNO
         MOVE SPACE         TO  EDIT-CURSOR OF STNO
     END-IF.
*↑2016/12/21
*
     MOVE     "Y"            TO   R002.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
 MAIN-040.
*
***  MOVE     "R002"         TO   DSP-GRP.
***  PERFORM   DSP-WR-SEC.
*確認入力
     MOVE      PMSG03        TO   MSG2.
     MOVE     "MSG2"         TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "R002"         TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE    DSP-FNC
         WHEN      "F004"
                             MOVE   SPACE   TO  MEISAI    R002
                             MOVE  "SCREEN" TO  DSP-GRP
                             PERFORM   DSP-WR-SEC
                             INITIALIZE     SHOCD  TAN1   TAN2
                                            TAN3   SHON1  SHON2
                                            KANA1  KANA2  GENKA
                                            GENTAN URITAN KUBUN1
                                            KUBUN2 SIRCD  JANCD
                                            IRESU  SIRNM  HATKBN
                                            TEIBAN KISETU KINAME
                                            R002   HAIBAN
                                            KANRI  BUNRUI BUNMEI
                             GO        TO   MAIN-020
         WHEN      "F009"
           MOVE     SPACE              TO   R002
           MOVE     "R002"             TO   DSP-GRP
           PERFORM   DSP-WR-SEC
           IF  (SYORI  = 1 OR 2)
                             GO        TO   MAIN-030
           ELSE
                             GO        TO   MAIN-040
           END-IF
         WHEN      "E000"
                             MOVE    SPACE   TO   SIRNM
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO   TO   MAIN-040
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*
     IF  (R002  NOT =  "Y")
           MOVE    MSG06    TO   MSG1
           MOVE   "MSG1"    TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO               TO   MAIN-040
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
              MOVE     SYS-DATE   TO  MEI-F98
              MOVE     MEI-REC    TO  DATA-TAIHI
              WRITE    MEI-REC
              END-WRITE
         WHEN     2
              MOVE     MEI-REC    TO  DATA-TAIHI
              REWRITE  MEI-REC
              END-REWRITE
         WHEN     3
              MOVE     MEI-REC    TO  DATA-TAIHI
              DELETE   HMEIMS
              END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
*94.03.09
     MOVE     SPACE     TO        R002.
*
     MOVE     SPACE        TO     MEISAI    R002.
     MOVE    "SCREEN"      TO     DSP-GRP.
     PERFORM  DSP-WR-SEC.
     INITIALIZE     SHOCD  TAN1   TAN2   TAN3   SHON1  SHON2
                    KANA1  KANA2  GENKA  GENTAN URITAN IRESU
           HATKBN   KUBUN1 KUBUN2 SIRCD  JANCD  R002  SIRNM
           TEIBAN   KISETU KINAME HAIBAN KANRI  BUNRUI BUNMEI.
     GO                    TO     MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  条件ファイル検索処理  2011/10/14 追加
****************************************************************
 HJYOKEN-RD-SEC               SECTION.
     READ  HJYOKEN
       INVALID
         MOVE  1            TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HJYOKEN-INV
     END-READ.

 HJYOKEN-RD-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HMEIMS     ZSHIMS1  MSTLOGF
                       DSPFILE    HJYOKEN.
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
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FIT00901.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FIT00901.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FIT00901"          TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FIT00901.
     INITIALIZE             FIT00901.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*            商品Ｍのレコードを画面に転送            *
****************************************************************
 DSP-TEN1-SEC              SECTION.
*↓2019.03.15
     MOVE     MEI-REC        TO        BEFORE-DATA.
*↑2019.03.15
     MOVE     MEI-F011       TO        SHOCD.
     MOVE     MEI-F0121      TO        TAN1.
     MOVE     MEI-F0122      TO        TAN2.
     MOVE     MEI-F0123      TO        TAN3.
     MOVE     MEI-F021       TO        SHON1.
     MOVE     MEI-F022       TO        SHON2.
     MOVE     MEI-F031       TO        KANA1.
     MOVE     MEI-F032       TO        KANA2.
     MOVE     MEI-F041       TO        GENKA.
     MOVE     MEI-F042       TO        GENTAN.
     MOVE     MEI-F043       TO        URITAN.
     MOVE     MEI-F93        TO        KUBUN1.
     MOVE     MEI-F92        TO        HATKBN.
     MOVE     MEI-F94        TO        KUBUN2.
     MOVE     MEI-F05        TO        SIRCD.
*93.11.19
     MOVE     MEI-F05        TO        SHI-F01.
     READ     ZSHIMS1
          INVALID
              MOVE    SPACE  TO        SIRNM
          NOT INVALID
              MOVE    SHI-F02  TO      SIRNM
     END-READ.
*
     MOVE     MEI-F06        TO        JANCD.
     MOVE     MEI-F07        TO        IRESU.
*00.07.11     定番区分、季節区分追加
     MOVE     MEI-F95        TO        TEIBAN.
     MOVE     MEI-F96        TO        KISETU.
     MOVE     SPACE          TO        JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     81             TO        JYO-F01.
     MOVE     MEI-F96        TO        JYO-F02(1:1).
     READ     HJYOKEN
          INVALID
              MOVE    SPACE       TO   KINAME
          NOT INVALID
              MOVE    JYO-F14     TO   KINAME
     END-READ.
*00.10.12     廃盤区分追加
     MOVE     MEI-F08        TO        HAIBAN.
*
*09.09.01     分類区分・管理区分を追加
     MOVE     SPACE          TO        JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     91             TO        JYO-F01.
     MOVE     MEI-F90        TO        JYO-F02.
     READ     HJYOKEN
          INVALID
              MOVE    SPACE    TO      BUNMEI
          NOT INVALID
              MOVE    JYO-F03  TO      BUNMEI
     END-READ.
     MOVE     MEI-F90        TO        BUNRUI.
     MOVE     MEI-F91        TO        KANRI.
* 2011/10/14,S  S.I/NAV
*サカタ２０分類
     MOVE  MEI-F09          TO  BNR20.

     MOVE  10               TO  JYO-F01.
     MOVE  MEI-F09          TO  JYO-F02.
     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-INV = 1
         MOVE  SPACE        TO  BNR20N
     ELSE
         MOVE JYO-F03       TO  BNR20N
     END-IF.
*小売区分
     MOVE  MEI-F10          TO  KOURI.
* 2011/10/14,E  S.I/NAV
* 2012/10/09,S  T.T/NAV
*物流束区分
     MOVE  MEI-F89          TO  BUTUR.
* 2012/10/09,E  T.T/NAV
*
*↓2016/12/21
*　振替商品区分
     MOVE  MEI-FIL1         TO  FURIKA.
*↑2016/12/21
*↓2020/07/09
*　ストックＮＯ管理区分
     MOVE  MEI-F97          TO  STNO.
*↑2020/07/09
*
 DSP-TEN1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TEN2-SEC           SECTION.
     MOVE     SHOCD          TO        MEI-F011.
     MOVE     TAN1           TO        MEI-F0121.
     MOVE     TAN2           TO        MEI-F0122.
     MOVE     TAN3           TO        MEI-F0123.
     MOVE     SHON1          TO        MEI-F021.
     MOVE     SHON2          TO        MEI-F022.
     MOVE     KANA1          TO        MEI-F031.
     MOVE     KANA2          TO        MEI-F032.
     MOVE     GENKA          TO        MEI-F041.
     MOVE     GENTAN         TO        MEI-F042.
     MOVE     URITAN         TO        MEI-F043.
     MOVE     KUBUN1         TO        MEI-F93.
     MOVE     HATKBN         TO        MEI-F92.
     MOVE     KUBUN2         TO        MEI-F94.
     MOVE     SIRCD          TO        MEI-F05.
     MOVE     JANCD          TO        MEI-F06.
     MOVE     IRESU          TO        MEI-F07.
     MOVE     TEIBAN         TO        MEI-F95.
     MOVE     KISETU         TO        MEI-F96.
*    廃盤区分追加 00/10/12
     MOVE     HAIBAN         TO        MEI-F08.
     MOVE     SYS-DATE       TO        MEI-F99.
*　　分類区分・官理区分追加 09/08/17
     MOVE     BUNRUI         TO        MEI-F90.
     MOVE     KANRI          TO        MEI-F91.
* 2011/10/14,S  S.I/NAV
     MOVE  BNR20            TO  MEI-F09. *> サカタ20分類
     MOVE  KOURI            TO  MEI-F10. *> 小売連携区分
* 2011/10/14,E  S.I/NAV
* 2012/10/09,S  T.T/NAV
     MOVE  BUTUR            TO  MEI-F89. *> 物流束区分
* 2012/10/09,E  T.T/NAV
*
*↓2016/12/21
     MOVE  FURIKA           TO  MEI-FIL1. *> 振替商品区分
*↑2016/12/21
*↓2020/07/09
     MOVE  STNO             TO  MEI-F97.  *> ストックＮＯ管理
*↑2020/07/09
*
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
     MOVE     "04"                TO   MSL-F01.
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
              MOVE     "04"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     SYORI         TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO    MSL-F07
              MOVE     DATA-TAIHI    TO    MSL-F08
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
     IF  (MSL-F01 > "04")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > SYORI)      OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "04"             TO  MSL-F01
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
         GO       TO               MSTLOG-WRITE-010
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
