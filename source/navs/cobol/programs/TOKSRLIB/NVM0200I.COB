***************************************************************
*
*   新．商品名称マスタ保守
*     2020/04/06  INOUE    商品名称マスタ・サブ商品名称マスタ
*                          を同時にメンテナンスする
*   更新履歴 ：
*     2020/05/27  INOUE    オーダー区分追加
*     2022/01/24  INOUE    ストックＮＯ管理区分追加
*
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NVM0200I.
*                     流用:SIT0095I.TOKSLIBS（コア）
*                     流用:NVM0100I.TOKSRLIB（制御）
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2020/04/06.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          PG6000.
 OBJECT-COMPUTER.          PG6000.
 SPECIAL-NAMES.
     STATION     IS        STA.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           FILE      STATUS    MEI-ST.
*サブ商品名称マスタ
     SELECT      SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   SUB-F011
                                                    SUB-F0121
                                                    SUB-F0122
                                                    SUB-F0123
                           FILE      STATUS    IS   SUB-ST.
*サブ商品名称マスタＬ７（同一D365JANCD存在チェック用）
     SELECT      SUBMEIL7  ASSIGN    TO        DA-01-VI-SUBMEIL7
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   SB7-D01
                           FILE      STATUS    IS   SB7-ST.
*サブ商品名称マスタＬ８（同一D365商品CD存在チェック用）
     SELECT      SUBMEIL8  ASSIGN    TO        DA-01-VI-SUBMEIL8
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   SB8-D02
                           FILE      STATUS    IS   SB8-ST.
*仕入先マスタ
     SELECT      ZSHIMS1   ASSIGN    TO        DA-01-VI-ZSHIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       SHI-F01.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   RANDOM
                           RECORD    KEY       IS   JYO-F01
                                                    JYO-F02
                           FILE      STATUS    IS   JYO-ST.
*マスタ更新履歴ファイル
     SELECT      MSTLOGF   ASSIGN    TO        DA-01-VI-MSTLOGL1
                           ORGANIZATION        IS   INDEXED
                           ACCESS    MODE      IS   DYNAMIC
                           RECORD    KEY       IS   MSL-F01
                                                    MSL-F02
                                                    MSL-F03
                                                    MSL-F04
                                                    MSL-F05
                                                    MSL-F06
                                                    MSL-F07
                           FILE      STATUS    IS   MSL-ST.
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
*商品名称マスタ
 FD  HMEIMS.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIF.
     COPY        SUBMEIF   OF        XFDLIB
                 JOINING   SUB       PREFIX.
*サブ商品名称マスタL7
 FD  SUBMEIL7.
     COPY        SUBMEIF   OF        XFDLIB
                 JOINING   SB7       PREFIX.
*サブ商品名称マスタL8
 FD  SUBMEIL8.
     COPY        SUBMEIF   OF        XFDLIB
                 JOINING   SB8       PREFIX.
*仕入先マスタ
 FD  ZSHIMS1.
     COPY        ZSHIMS    OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*画面
 FD  DSPFILE.
 01  DSP-REC            PIC  X(2000).
     COPY     FVM02001  OF   XMDLIB.
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
     03  MSG01           PIC N(20) VALUE
                         NC"登録されていません".
     03  MSG02           PIC N(20) VALUE
                         NC"すでに登録されています".
     03  MSG03           PIC N(20) VALUE
                         NC"Ｄ３６５商品ＣＤが既に存在します".
     03  MSG04           PIC N(20) VALUE
                         NC"商品コードを入力してください".
     03  MSG05           PIC N(20) VALUE
                         NC"処理区分が違います".
     03  MSG06           PIC N(20) VALUE
                         NC"Ｙで入力して下さい".
     03  MSG10           PIC N(20) VALUE
                         NC"ＰＦキーが違います".
     03  MSG11           PIC N(20) VALUE
                         NC"仕入先が未登録です".
     03  MSG12           PIC N(20) VALUE
                         NC"定番区分に誤りがあります".
     03  MSG13           PIC N(20) VALUE
                         NC"Ｄ３６５ＪＡＮＣＤが既に存在します".
     03  MSG14           PIC N(20) VALUE
                         NC"必須入力項目です".
     03  MSG15           PIC N(20) VALUE
                         NC"廃盤区分に誤りがあります".
     03  MSG16           PIC N(20) VALUE
                         NC"分類区分に誤りがあります".
     03  MSG17           PIC N(20) VALUE
                         NC"管理区分に誤りがあります".
     03  MSG18           PIC N(20) VALUE
                         NC"サカタ２０分類に誤りがあります".
     03  MSG19           PIC N(20) VALUE
                         NC"小売連携区分に誤りがあります".
     03  MSG20           PIC N(20) VALUE
                         NC"物流束区分に誤りがあります".
     03  MSG21           PIC N(20) VALUE
                         NC"振替商品区分に誤りがあります".
*↓2021/01/24
     03  MSG22           PIC N(20) VALUE
                       NC"ストックＮＯ管理区分に誤りがあります".
*↑2021/01/24
*
     03  PMSG01            PIC N(20) VALUE
*----次検索なし------------NC"_取消　_次検索".
                           NC"_取消　　　　　".
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
 01  WK-MEI-F98                   PIC  9(08)  VALUE  ZERO.
 01  WK-MEI-F99                   PIC  9(08)  VALUE  ZERO.
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
     03  INV-SW            PIC 9(01)   VALUE   ZERO.
     03  ZI-FLG            PIC 9(01)   VALUE   ZERO.
     03  WK-SYORI          PIC 9(01)   VALUE   ZERO.
     03  HMEIMS-INV-FLG    PIC  X(03)  VALUE   SPACE.
     03  SUBMEIF-INV-FLG   PIC  X(03)  VALUE   SPACE.
     03  FILE-KBN          PIC  X(01)  VALUE   SPACE.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  MSL-ST            PIC X(02).
     03  SUB-ST            PIC X(02).
     03  SB7-ST            PIC X(02).
     03  SB8-ST            PIC X(02).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"商品名称マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"条件ファイル異常！".
     03  FILE-ERR4         PIC N(12) VALUE
                                  NC"マスタ更新履歴Ｆ異常！".
     03  FILE-ERR5         PIC N(12) VALUE
                                  NC"サブ商品名称マスタ異常！".
 01  IXD                   PIC 9(06) VALUE ZERO.
 01  IXB                   PIC 9(06) VALUE ZERO.
 01  IXC                   PIC 9(06) VALUE ZERO.
 01  FG-HJYOKEN-INV        PIC 9(01).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
*01  BEFORE-DATA           PIC  X(170).
*01  DATA-TAIHI            PIC  X(170).
*01  BEFORE-DATA           PIC  X(512).
*01  DATA-TAIHI            PIC  X(512).
 01  BEFORE-DATA.
   02  BEFOR-F01.
     03  BEFOR-F011        PIC  X(08).
     03  BEFOR-F012.
       05  BEFOR-F0121     PIC  X(05).
       05  BEFOR-F0122     PIC  X(02).
       05  BEFOR-F0123     PIC  X(01).
   02  BEFOR-F02.
     03  BEFOR-F021        PIC  N(15).
     03  BEFOR-F022        PIC  N(15).
   02  BEFOR-F03.
     03  BEFOR-F031        PIC  X(15).
     03  BEFOR-F032        PIC  X(15).
   02  BEFOR-F04.
     03  BEFOR-F041        PIC  9(07)V99 PACKED-DECIMAL.
     03  BEFOR-F042        PIC  9(07)V99 PACKED-DECIMAL.
     03  BEFOR-F043        PIC  9(07)V99 PACKED-DECIMAL.
   02  BEFOR-F05           PIC  X(08).
   02  BEFOR-F06           PIC  X(13).
   02  BEFOR-F07           PIC  9(04)V99 PACKED-DECIMAL.
   02  BEFOR-F08           PIC  X(01).
   02  BEFOR-F09           PIC  X(02).
   02  BEFOR-F10           PIC  X(01).
   02  BEFOR-FIL1          PIC  X(01).
   02  BEFOR-F89           PIC  X(01).
   02  BEFOR-KUBUN.
     03  BEFOR-F90         PIC  X(01).
     03  BEFOR-F91         PIC  X(01).
     03  BEFOR-F92         PIC  9(01).
     03  BEFOR-F93         PIC  9(01).
     03  BEFOR-F94         PIC  9(01).
     03  BEFOR-F95         PIC  9(01).
     03  BEFOR-F96         PIC  X(01).
     03  BEFOR-F97         PIC  9(01).
   02  BEFOR-F98           PIC  9(08) PACKED-DECIMAL.
   02  BEFOR-F99           PIC  9(08) PACKED-DECIMAL.
   02  BEFOR-D00.
     03  BEFOR-D01         PIC  X(20).
     03  BEFOR-D02         PIC  X(20).
*2020/05/27↓
     03  BEFOR-D03         PIC  X(10).
*    03  BEFOR-DFIL1       PIC  X(500).
     03  BEFOR-DFIL1       PIC  X(490).
*2020/05/27↑
     03  BEFOR-D92         PIC  X(04).
     03  BEFOR-D93         PIC  X(02).
     03  BEFOR-D94         PIC  9(08).
     03  BEFOR-D95         PIC  9(06).
     03  BEFOR-D96         PIC  X(04).
     03  BEFOR-D97         PIC  X(02).
     03  BEFOR-D98         PIC  9(08).
     03  BEFOR-D99         PIC  9(06).
*
 01  DATA-TAIHI.
   02  AFTER-F01.
     03  AFTER-F011        PIC  X(08).
     03  AFTER-F012.
       05  AFTER-F0121     PIC  X(05).
       05  AFTER-F0122     PIC  X(02).
       05  AFTER-F0123     PIC  X(01).
   02  AFTER-F02.
     03  AFTER-F021        PIC  N(15).
     03  AFTER-F022        PIC  N(15).
   02  AFTER-F03.
     03  AFTER-F031        PIC  X(15).
     03  AFTER-F032        PIC  X(15).
   02  AFTER-F04.
     03  AFTER-F041        PIC  9(07)V99 PACKED-DECIMAL.
     03  AFTER-F042        PIC  9(07)V99 PACKED-DECIMAL.
     03  AFTER-F043        PIC  9(07)V99 PACKED-DECIMAL.
   02  AFTER-F05           PIC  X(08).
   02  AFTER-F06           PIC  X(13).
   02  AFTER-F07           PIC  9(04)V99 PACKED-DECIMAL.
   02  AFTER-F08           PIC  X(01).
   02  AFTER-F09           PIC  X(02).
   02  AFTER-F10           PIC  X(01).
   02  AFTER-FIL1          PIC  X(01).
   02  AFTER-F89           PIC  X(01).
   02  AFTER-KUBUN.
     03  AFTER-F90         PIC  X(01).
     03  AFTER-F91         PIC  X(01).
     03  AFTER-F92         PIC  9(01).
     03  AFTER-F93         PIC  9(01).
     03  AFTER-F94         PIC  9(01).
     03  AFTER-F95         PIC  9(01).
     03  AFTER-F96         PIC  X(01).
     03  AFTER-F97         PIC  9(01).
   02  AFTER-F98           PIC  9(08) PACKED-DECIMAL.
   02  AFTER-F99           PIC  9(08) PACKED-DECIMAL.
   02  AFTER-D00.
     03  AFTER-D01         PIC  X(20).
     03  AFTER-D02         PIC  X(20).
*2020/05/27↓
     03  AFTER-D03         PIC  X(10).
*    03  AFTER-DFIL1       PIC  X(500).
     03  AFTER-DFIL1       PIC  X(490).
*2020/05/27↑
     03  AFTER-D92         PIC  X(04).
     03  AFTER-D93         PIC  X(02).
     03  AFTER-D94         PIC  9(08).
     03  AFTER-D95         PIC  9(06).
     03  AFTER-D96         PIC  X(04).
     03  AFTER-D97         PIC  X(02).
     03  AFTER-D98         PIC  9(08).
     03  AFTER-D99         PIC  9(06).
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
     DISPLAY     FILE-ERR1 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SUB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SUBMEIF.
     DISPLAY     SUB-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SB7-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SUBMEIL7.
     DISPLAY     SB7-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SB8-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SUBMEIL8.
     DISPLAY     SB8-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
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
     OPEN        I-O       HMEIMS  SUBMEIF  MSTLOGF
                           DSPFILE.
     OPEN        INPUT     ZSHIMS1 SUBMEIL7 SUBMEIL8
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
*----------------------------------------------------------
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE  MEI-REC.
     MOVE        SPACE     TO        SUB-REC.
     INITIALIZE  SUB-REC.
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
*
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
*
*処理区分チェック
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
         IF      SYORI     =   1
                 MOVE  1   TO        WK-SYORI
         END-IF.
         IF      SYORI     =   2
                 MOVE  2   TO        WK-SYORI
         END-IF.
         IF      SYORI     =   3
                 MOVE  3   TO        WK-SYORI
         END-IF.
*
 MAIN-020.
*--< ＰＦキー選択 >--*
     IF  (SYORI  =  2 OR 3)
         MOVE    PMSG01    TO        MSG2
     ELSE
         MOVE    PMSG02    TO        MSG2
     END-IF.
*
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
*
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           GO              TO        MAIN-SEC
*      WHEN
*       "F010"
*          IF  (SYORI  =  2)  AND  (ZI-FLG = ZERO)
*              IF  (INV-SW = 1)
*                  MOVE        SHOCD     TO        MEI-F011
*                  MOVE        TAN1      TO        MEI-F0121
*                  MOVE        TAN2      TO        MEI-F0122
*                  MOVE        TAN3      TO        MEI-F0123
*                  START     HMEIMS  KEY IS  >
*                       MEI-F011  MEI-F0121  MEI-F0122 MEI-F0123
*                  INVALID  KEY
*                       MOVE      NC"次レコード無し"  TO MSG1
*                       MOVE        "MSG1"     TO        DSP-GRP
*                       PERFORM      DSP-WR-SEC
*                       MOVE         1         TO        ZI-FLG
*                       MOVE         1         TO        INV-SW
*                       GO                     TO        MAIN-020
*                  END-START
*              END-IF
*              READ  HMEIMS    NEXT
*                AT  END
*                  MOVE NC"次レコード無し"     TO        MSG1
*                  MOVE   "MSG1"     TO        DSP-GRP
*                  PERFORM DSP-WR-SEC
*                  MOVE    1         TO        ZI-FLG
*                  GO                TO        MAIN-020
*                NOT AT END
*                  MOVE    ZERO      TO        INV-SW
*                  GO      TO        MAIN-000
*              END-READ
*          END-IF
*          IF  (SYORI  =  2)
*              MOVE NC"次レコード無し"     TO        MSG1
*          ELSE
*              MOVE    MSG10   TO    MSG1
*          END-IF
*          MOVE    "MSG1"      TO    DSP-GRP
*          PERFORM  DSP-WR-SEC
*          GO                  TO    MAIN-020
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
*
*商品コード・品単コードチェック＆マスタ存在チェック
 MAIN-001.
*
*      商品ＣＤ右詰め処理
       IF      SHOCD          NOT =     SPACE
            PERFORM VARYING IXB    FROM  1   BY    1
                    UNTIL   IXB    >     7
                    PERFORM VARYING IXC      FROM  8   BY  -1
                            UNTIL   IXC      <     2
                      IF    SHOCD  (IXC:1)   =     SPACE
                            COMPUTE IXD      =     IXC   -  1
                            MOVE  SHOCD  (IXD:1)   TO
                                                SHOCD      (IXC:1)
                            MOVE  SPACE            TO
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
*
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
         PERFORM DSP-WR-SEC
     END-IF.
*------------------------------------
*    存在チェック　→　状態別制御準備
*------------------------------------
*-   MOVE        ZERO      TO        ZI-FLG.
*-   MOVE        SHOCD     TO        MEI-F011.
*-   MOVE        TAN1      TO        MEI-F0121.
*-   MOVE        TAN2      TO        MEI-F0122.
*-   MOVE        TAN3      TO        MEI-F0123.
*-   READ  HMEIMS
*-     INVALID
*-       IF  (SYORI  =  1)
*-           MOVE    1         TO        INV-SW
*-           MOVE    SPACE     TO        MSG1
*-           MOVE   "MSG1"     TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           MOVE   "M"        TO        EDIT-OPTION  OF  SHOCD
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  SHOCD
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN1
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN1
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN2
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN2
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN3
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN3
*-           MOVE   "GRPKEY"   TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           GO                TO        MAIN-030
*-       ELSE
*-           MOVE    MSG01     TO        MSG1
*-           MOVE    1         TO        INV-SW
*-           MOVE   "MSG1"     TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           MOVE   "R"        TO        EDIT-OPTION  OF  SHOCD
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  SHOCD
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN1
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN1
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN2
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN2
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN3
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN3
*-           MOVE   "GRPKEY"   TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           GO                TO        MAIN-020
*-       END-IF
*-     NOT INVALID
*-       IF  (SYORI  =  1)
*-           MOVE    MSG02     TO        MSG1
*-           MOVE   "MSG1"     TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           MOVE   "R"        TO        EDIT-OPTION  OF  SHOCD
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  SHOCD
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN1
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN1
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN2
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN2
*-           MOVE   "R"        TO        EDIT-OPTION  OF  TAN3
*-           MOVE   "C"        TO        EDIT-CURSOR  OF  TAN3
*-           MOVE   "GRPKEY"   TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-           GO                TO        MAIN-020
*-        ELSE
*-           MOVE   "M"        TO        EDIT-OPTION  OF  SHOCD
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  SHOCD
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN1
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN1
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN2
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN2
*-           MOVE   "M"        TO        EDIT-OPTION  OF  TAN3
*-           MOVE    SPACE     TO        EDIT-CURSOR  OF  TAN3
*-           MOVE   "GRPKEY"   TO        DSP-GRP
*-           PERFORM DSP-WR-SEC
*-       END-IF
*-   END-READ.
*----
     MOVE      SHOCD         TO     MEI-F011   SUB-F011.
     MOVE      TAN1          TO     MEI-F0121  SUB-F0121.
     MOVE      TAN2          TO     MEI-F0122  SUB-F0122.
     MOVE      TAN3          TO     MEI-F0123  SUB-F0123.
     PERFORM   HMEIMS-READ-SEC.
     PERFORM   SUBMEIF-READ-SEC.
*
     EVALUATE  WK-SYORI ALSO SUBMEIF-INV-FLG ALSO HMEIMS-INV-FLG
*
*        モード：登録　サブ：あり　マスタ：あり
         WHEN  "1"  ALSO  SPACE  ALSO  SPACE *> 登録済エラー
               MOVE    MSG02     TO    MSG1
               MOVE   "MSG1"     TO    DSP-GRP
               PERFORM DSP-WR-SEC
               MOVE   "R"        TO    EDIT-OPTION  OF  SHOCD
               MOVE   "C"        TO    EDIT-CURSOR  OF  SHOCD
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN1
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN1
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN2
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN2
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN3
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN3
               MOVE   "GRPKEY"   TO    DSP-GRP
               PERFORM DSP-WR-SEC
               GO                TO    MAIN-020
*
*        モード：登録　サブ：あり　マスタ：なし
         WHEN  "1"  ALSO  SPACE  ALSO "INV"  *> サブより
               MOVE   "1"        TO    FILE-KBN
*
*        モード：登録　サブ：なし　マスタ：あり
         WHEN  "1"  ALSO "INV"   ALSO  SPACE *> マスタより
               MOVE   "2"        TO    FILE-KBN
*
*        モード：登録　サブ：なし　マスタ：なし
         WHEN  "1"  ALSO "INV"   ALSO "INV"  *> 初期値
               MOVE   "3"        TO    FILE-KBN
*
*        モード：修正　サブ：あり　マスタ：あり
         WHEN  "2"  ALSO SPACE   ALSO  SPACE *> サブより
               MOVE   "4"        TO    FILE-KBN
*
*        モード：修正　サブ：あり　マスタ：なし
         WHEN  "2"  ALSO SPACE   ALSO "INV"  *> サブより
               MOVE   "1"        TO    FILE-KBN
*        モード：修正　サブ：なし　マスタ：あり
         WHEN  "2"  ALSO "INV"   ALSO  SPACE *> マスタより
               MOVE   "2"        TO    FILE-KBN
*
*        モード：修正　サブ：なし　マスタ：なし
         WHEN  "2"  ALSO "INV"   ALSO "INV"  *> 未登録エラー
               MOVE    MSG01     TO    MSG1
               MOVE    1         TO    INV-SW
               MOVE   "MSG1"     TO    DSP-GRP
               PERFORM DSP-WR-SEC
               MOVE   "R"        TO    EDIT-OPTION  OF  SHOCD
               MOVE   "C"        TO    EDIT-CURSOR  OF  SHOCD
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN1
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN1
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN2
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN2
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN3
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN3
               MOVE   "GRPKEY"   TO    DSP-GRP
               PERFORM DSP-WR-SEC
               GO                TO    MAIN-020
*
*        モード：削除　サブ：あり　マスタ：あり
         WHEN  "3"  ALSO SPACE   ALSO  SPACE *> サブより
               MOVE   "4"        TO    FILE-KBN
*
*        モード：削除　サブ：あり　マスタ：なし
         WHEN  "3"  ALSO SPACE   ALSO "INV"  *> サブより
               MOVE   "1"        TO    FILE-KBN
*
*        モード：削除　サブ：なし　マスタ：あり
         WHEN  "3"  ALSO "INV"   ALSO  SPACE *> 未登録エラー
               MOVE    MSG01     TO    MSG1
               MOVE    1         TO    INV-SW
               MOVE   "MSG1"     TO    DSP-GRP
               PERFORM DSP-WR-SEC
               MOVE   "R"        TO    EDIT-OPTION  OF  SHOCD
               MOVE   "C"        TO    EDIT-CURSOR  OF  SHOCD
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN1
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN1
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN2
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN2
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN3
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN3
               MOVE   "GRPKEY"   TO    DSP-GRP
               PERFORM DSP-WR-SEC
               GO                TO    MAIN-020
*
*        モード：削除　サブ：なし　マスタ：なし
         WHEN  "3"  ALSO "INV"  ALSO  "INV"  *> 未登録エラー
               MOVE    MSG01     TO    MSG1
               MOVE    1         TO    INV-SW
               MOVE   "MSG1"     TO    DSP-GRP
               PERFORM DSP-WR-SEC
               MOVE   "R"        TO    EDIT-OPTION  OF  SHOCD
               MOVE   "C"        TO    EDIT-CURSOR  OF  SHOCD
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN1
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN1
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN2
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN2
               MOVE   "R"        TO    EDIT-OPTION  OF  TAN3
               MOVE   "C"        TO    EDIT-CURSOR  OF  TAN3
               MOVE   "GRPKEY"   TO    DSP-GRP
               PERFORM DSP-WR-SEC
               GO                TO    MAIN-020
*
     END-EVALUATE.
*
*--------------------------------------------
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
*    IF  (SYORI  =  2 OR 3)
*        PERFORM     HMEIMS-SET-SEC
*        MOVE       "GRPKEY"   TO        DSP-GRP
*        PERFORM     DSP-WR-SEC
*        MOVE       "GRP001"   TO        DSP-GRP
*        PERFORM     DSP-WR-SEC
*    END-IF.
*
*　表示項目セット
*　  サブより
     IF  FILE-KBN  =  "1"  OR  "4"
         PERFORM SUBMEIF-SET-SEC
     END-IF.
*
*　  マスタより
     IF  FILE-KBN  =  "2"
         PERFORM HMEIMS-SET-SEC
     END-IF.
*
*  表示
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
*  削除モード：確認へジャンプ
     IF  SYORI  =  3
         GO                TO        MAIN-040
     END-IF.
*-----------------------------------------------
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
                                                ADDTAN  ADDDAT
                                                UPDTAN  UPDDAT
                             MOVE  "SCREEN" TO  DSP-GRP
                             PERFORM   DSP-WR-SEC
                             INITIALIZE     SHOCD  TAN1   TAN2
                                            TAN3   SHON1  SHON2
                                            KANA1  KANA2  GENKA
                                            GENTAN URITAN JANCD
                                            SIRCD  D365CD D365JN
                                            IRESU  SIRNM
                                            TEIBAN HAIBAN R002
                                            ADDTAN ADDDAT
                                            UPDTAN UPDDAT
*2020/05/27↓
                                            ODKBN
*2020/05/27↑
*2022/01/24↓
                                            STNO
*2022/01/24↑
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
*    IF  (KUBUN1  =  0 OR 1)
*        MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN1
*        MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN1
***      MOVE     "GRP001"   TO   DSP-GRP
***      PERFORM   DSP-WR-SEC
*      ELSE
*        MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN1
*        MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN1
*        MOVE      MSG03     TO   MSG1
***      MOVE     "MSG1"     TO   DSP-GRP
***      PERFORM   DSP-WR-SEC
***      MOVE     "GRP001"   TO   DSP-GRP
*        MOVE     "SCREEN"   TO   DSP-GRP
*        PERFORM   DSP-WR-SEC
*        GO   TO   MAIN-030
*    END-IF.
*
*Ｄ３６５商品ＣＤ
     IF  D365CD    NOT =  SPACE
         MOVE     "M"        TO   EDIT-OPTION  OF  D365CD
         MOVE      SPACE     TO   EDIT-CURSOR  OF  D365CD
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  D365CD
         MOVE     "C"        TO   EDIT-CURSOR  OF  D365CD
         MOVE      MSG14     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
     MOVE     "M"        TO   EDIT-OPTION  OF  D365CD.
     MOVE     SPACE      TO   EDIT-CURSOR  OF  D365CD.
     MOVE     D365CD     TO   SB8-D02.
     READ     SUBMEIL8
         INVALID
              CONTINUE
         NOT INVALID
*T
*             DISPLAY "D365CD=" D365CD     UPON STA
*             DISPLAY "SUB  ="  SB8-D02    UPON STA
*             DISPLAY "SHOCD="  SHOCD      UPON STA
*             DISPLAY "SUB  ="  SB8-F011   UPON STA
*             DISPLAY "TAN1 ="  TAN1       UPON STA
*             DISPLAY "SUB  ="  SB8-F0121  UPON STA
*             DISPLAY "TAN2 ="  TAN2       UPON STA
*             DISPLAY "SUB  ="  SB8-F0122  UPON STA
*             DISPLAY "TAN3 ="  TAN3       UPON STA
*             DISPLAY "SUB  ="  SB8-F0123  UPON STA
*T
              IF  ( SHOCD = SB8-F011  ) AND
                  ( TAN1  = SB8-F0121 ) AND
                  ( TAN2  = SB8-F0122 ) AND
                  ( TAN3  = SB8-F0123 )
                    CONTINUE
              ELSE
                    MOVE   "R"      TO   EDIT-OPTION  OF  D365CD
                    MOVE   "C"      TO   EDIT-CURSOR  OF  D365CD
                    MOVE    MSG03   TO   MSG1
                    MOVE   "SCREEN" TO   DSP-GRP
                    PERFORM   DSP-WR-SEC
                    GO   TO   MAIN-030
              END-IF
     END-READ.
*Ｄ３６５ＪＡＮＣＤ
     IF  D365JN    NOT =  SPACE
         MOVE     "M"        TO   EDIT-OPTION  OF  D365JN
         MOVE      SPACE     TO   EDIT-CURSOR  OF  D365JN
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  D365JN
         MOVE     "C"        TO   EDIT-CURSOR  OF  D365JN
         MOVE      MSG14     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
     MOVE     "M"        TO   EDIT-OPTION  OF  D365JN.
     MOVE     SPACE      TO   EDIT-CURSOR  OF  D365JN.
     MOVE     D365JN     TO   SB7-D01.
     READ     SUBMEIL7
         INVALID
              CONTINUE
         NOT INVALID
*T
*             DISPLAY "D365JN=" D365JN     UPON STA
*             DISPLAY "SUB  ="  SB7-D01    UPON STA
*             DISPLAY "SHOCD="  SHOCD      UPON STA
*             DISPLAY "SUB  ="  SB7-F011   UPON STA
*             DISPLAY "TAN1 ="  TAN1       UPON STA
*             DISPLAY "SUB  ="  SB7-F0121  UPON STA
*             DISPLAY "TAN2 ="  TAN2       UPON STA
*             DISPLAY "SUB  ="  SB7-F0122  UPON STA
*             DISPLAY "TAN3 ="  TAN3       UPON STA
*             DISPLAY "SUB  ="  SB7-F0123  UPON STA
*T
              IF  ( SHOCD = SB7-F011  ) AND
                  ( TAN1  = SB7-F0121 ) AND
                  ( TAN2  = SB7-F0122 ) AND
                  ( TAN3  = SB7-F0123 )
                    CONTINUE
              ELSE
                    MOVE   "R"      TO   EDIT-OPTION  OF  D365JN
                    MOVE   "C"      TO   EDIT-CURSOR  OF  D365JN
                    MOVE    MSG13   TO   MSG1
                    MOVE   "SCREEN" TO   DSP-GRP
                    PERFORM   DSP-WR-SEC
                    GO   TO   MAIN-030
              END-IF
     END-READ.
*商品名１
     IF  SHON1     NOT =  SPACE
         MOVE     "M"        TO   EDIT-OPTION  OF  SHON1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SHON1
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  SHON1
         MOVE     "C"        TO   EDIT-CURSOR  OF  SHON1
         MOVE      MSG14     TO   MSG1
         MOVE     "SCREEN"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*
*仕入先コード
     MOVE     "M"        TO   EDIT-OPTION  OF  SIRCD.
     MOVE     SPACE      TO   EDIT-CURSOR  OF  SIRCD.
     MOVE     SIRCD      TO   SHI-F01.
     READ     ZSHIMS1
         INVALID
             MOVE     "R"        TO   EDIT-OPTION  OF  SIRCD
             MOVE     "C"        TO   EDIT-CURSOR  OF  SIRCD
             MOVE      MSG11     TO   MSG1
             MOVE     "SCREEN"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
         NOT INVALID
             MOVE      SHI-F02   TO   SIRNM
     END-READ.
*
*自動発注区分
*    IF  (HATKBN  =  0 OR 1)
*        MOVE     "M"        TO   EDIT-OPTION  OF  HATKBN
*        MOVE      SPACE     TO   EDIT-CURSOR  OF  HATKBN
*      ELSE
*        MOVE     "R"        TO   EDIT-OPTION  OF  HATKBN
*        MOVE     "C"        TO   EDIT-CURSOR  OF  HATKBN
*        MOVE      MSG14     TO   MSG1
*        MOVE     "SCREEN"   TO   DSP-GRP
*        PERFORM   DSP-WR-SEC
*        GO   TO   MAIN-030
*    END-IF.
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
*    IF  KISETU    NOT =     SPACE
*        MOVE      81             TO   JYO-F01
*        MOVE      KISETU         TO   JYO-F02
*        READ      HJYOKEN
*             INVALID
*                  MOVE      SPACE     TO   KINAME
*                  MOVE  "R"  TO   EDIT-OPTION  OF  KISETU
*                  MOVE  "C"  TO   EDIT-CURSOR  OF  KISETU
*                  MOVE   MSG13     TO   MSG1
*                  MOVE  "SCREEN"   TO   DSP-GRP
*                  PERFORM   DSP-WR-SEC
*                  GO   TO   MAIN-030
*             NOT INVALID
*                  MOVE      JYO-F14   TO   KINAME
*                  MOVE "M"       TO   EDIT-OPTION  OF  KISETU
*                  MOVE  SPACE    TO   EDIT-CURSOR  OF  KISETU
*        END-READ
*      ELSE
*        MOVE      SPACE     TO   KINAME
*        MOVE     "R"        TO   EDIT-OPTION  OF  KISETU
*        MOVE     "C"        TO   EDIT-CURSOR  OF  KISETU
*        MOVE      MSG13     TO   MSG1
*        MOVE     "SCREEN"   TO   DSP-GRP
*        PERFORM   DSP-WR-SEC
*        GO   TO   MAIN-030
*    END-IF.
*
*廃盤区分
     IF  HAIBAN    NOT =     SPACE
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
*分類区分
     IF  BUNRUI    NOT =     SPACE
         MOVE      91             TO   JYO-F01
         MOVE      BUNRUI         TO   JYO-F02
         READ      HJYOKEN
              INVALID
*-                 MOVE       SPACE     TO   BUNRUI
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
*-       MOVE     "R"        TO   EDIT-OPTION  OF  BUNRUI
*-       MOVE     "C"        TO   EDIT-CURSOR  OF  BUNRUI
*-       MOVE      MSG16     TO   MSG1
*-       MOVE     "SCREEN"   TO   DSP-GRP
*-       PERFORM   DSP-WR-SEC
*-       GO   TO   MAIN-030
         MOVE      "M"       TO   EDIT-OPTION  OF  BUNRUI
         MOVE      SPACE     TO   EDIT-CURSOR  OF  BUNRUI
     END-IF.
*
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
*
*サカタ２０分類
     IF    BNR20    NOT =       SPACE
           MOVE     10            TO  JYO-F01
           MOVE     BNR20         TO  JYO-F02
           PERFORM  HJYOKEN-RD-SEC
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
           END-IF
     ELSE
           MOVE SPACE             TO  BNR20N
           MOVE "M"               TO  EDIT-OPTION OF BNR20
           MOVE  SPACE            TO  EDIT-CURSOR OF BNR20
     END-IF.
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
*
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
*
*2020/05/27↓
*　オーダー区分
     IF  ODKBN      = SPACE
         MOVE  "R"       TO  EDIT-OPTION OF ODKBN
         MOVE  "C"       TO  EDIT-CURSOR OF ODKBN
         MOVE  MSG14     TO  MSG1
         MOVE  "SCREEN"  TO  DSP-GRP
         PERFORM  DSP-WR-SEC
         GO TO  MAIN-030
     ELSE
         MOVE "M"           TO  EDIT-OPTION OF ODKBN
         MOVE SPACE         TO  EDIT-CURSOR OF ODKBN
     END-IF.
*2020/05/27↑
*
*↓2022/01/24 ストックＮＯ管理区分追加
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
*↑2022/01/24
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
     IF        SYORI  =  3
               MOVE      PMSG02   TO   MSG2
     END-IF.
     MOVE     "MSG2"         TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "R002"         TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE    DSP-FNC
         WHEN      "F004"
                             MOVE   SPACE   TO  MEISAI    R002
                                                ADDTAN    ADDDAT
                                                UPDTAN    UPDDAT
                             MOVE  "SCREEN" TO  DSP-GRP
                             PERFORM   DSP-WR-SEC
                             INITIALIZE     SHOCD  TAN1   TAN2
                                            TAN3   D365CD D365JN
                                            SHON1  SHON2
                                            KANA1  KANA2  GENKA
                                            GENTAN URITAN
                                            SIRCD  JANCD
                                            IRESU  SIRNM
                                            TEIBAN HAIBAN KANRI
                                            BUNRUI BUNMEI R002
                                            ADDTAN ADDDAT
                                            UPDTAN UPDDAT
*2020/05/27↓
                                            ODKBN
*2020/05/27↑
*2022/01/24↓
                                            STNO
*2022/01/24↑
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
*
*画面から各マスタレコードへ項目転送
     PERFORM  DSP-TO-FILE-SEC.
*
*各マスタ更新＆履歴出力
 MAIN-080.
     MOVE       SPACE                   TO    DATA-TAIHI.
     EVALUATE   SYORI   ALSO     FILE-KBN
*        登録モード　＆　サブ：あり　＆　マスタ：なし
         WHEN   1       ALSO     "1"
                MOVE    SYS-DATE          TO  MEI-F98
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                WRITE   MEI-REC
                REWRITE SUB-REC
*        登録モード　＆　サブ：なし　＆　マスタ：あり
         WHEN   1       ALSO     "2"
                MOVE    SYS-DATE          TO  SUB-F98
                MOVE    PARA-BUMONCD      TO  SUB-D92
                MOVE    PARA-TANCD        TO  SUB-D93
                MOVE    SYS-DATE          TO  SUB-D94
                MOVE    WK-TIME(1:6)      TO  SUB-D95
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                REWRITE MEI-REC
                WRITE   SUB-REC
*        登録モード　＆　サブ：なし　＆　マスタ：なし
         WHEN   1       ALSO     "3"
                MOVE    SYS-DATE          TO  MEI-F98
                MOVE    SYS-DATE          TO  SUB-F98
                MOVE    PARA-BUMONCD      TO  SUB-D92
                MOVE    PARA-TANCD        TO  SUB-D93
                MOVE    SYS-DATE          TO  SUB-D94
                MOVE    WK-TIME(1:6)      TO  SUB-D95
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                WRITE   MEI-REC
                WRITE   SUB-REC
*        修正モード　＆　サブ：あり　＆　マスタ：なし
         WHEN   2       ALSO     "1"
                MOVE    SYS-DATE          TO  MEI-F98
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                WRITE   MEI-REC
                REWRITE SUB-REC
*        修正モード　＆　サブ：なし　＆　マスタ：あり
         WHEN   2       ALSO     "2"
                MOVE    SYS-DATE          TO  SUB-F98
                MOVE    PARA-BUMONCD      TO  SUB-D92
                MOVE    PARA-TANCD        TO  SUB-D93
                MOVE    SYS-DATE          TO  SUB-D94
                MOVE    WK-TIME(1:6)      TO  SUB-D95
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                REWRITE MEI-REC
                WRITE   SUB-REC
*        修正モード　＆　サブ：あり　＆　マスタ：あり
         WHEN   2       ALSO     "4"
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                REWRITE MEI-REC
                REWRITE SUB-REC
*        削除モード　＆　サブ：あり　＆　マスタ：なし
         WHEN   3       ALSO     "1"
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                DELETE  SUBMEIF
*        削除モード　＆　サブ：あり　＆　マスタ：あり
         WHEN   3       ALSO     "4"
*2020/05/27↓
*               MOVE    SUB-REC(1:210)    TO  DATA-TAIHI(1:210)
*               MOVE    SUB-REC(211:262)  TO  DATA-TAIHI(211:262)
*               MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
                MOVE    SUB-REC(1:220)    TO  DATA-TAIHI(1:220)
                MOVE    SUB-REC(221:252)  TO  DATA-TAIHI(221:252)
                MOVE    SUB-REC(711:40)   TO  DATA-TAIHI(473:40)
*2020/05/27↑
                DELETE  HMEIMS
                DELETE  SUBMEIF
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
* 画面クリア
     MOVE     SPACE        TO     MEISAI    R002
                                  ADDTAN    UPDTAN
                                  ADDDAT    UPDDAT.
     MOVE    "SCREEN"      TO     DSP-GRP.
     PERFORM  DSP-WR-SEC.
     INITIALIZE     SHOCD  TAN1   TAN2   TAN3   SHON1  SHON2
                    KANA1  KANA2  GENKA  GENTAN URITAN IRESU
                    SIRCD  JANCD  R002   SIRNM  TEIBAN HAIBAN
*↓2022/01/24
*                   KANRI  BUNRUI BUNMEI D365CD D365JN ODKBN.
                    KANRI  BUNRUI BUNMEI D365CD D365JN ODKBN
                    STNO.
*↑2022/01/24
     GO             TO     MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV" TO HMEIMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ読込
****************************************************************
 SUBMEIF-READ-SEC        SECTION.
*
     READ  SUBMEIF
           INVALID      MOVE "INV" TO SUBMEIF-INV-FLG
           NOT INVALID  MOVE SPACE TO SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
****************************************************************
*  条件ファイル検索処理
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
                       SUBMEIF    SUBMEIL7 SUBMEIL8
                       DSPFILE    HJYOKEN.
 END-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"              TO   DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面    ＷＲＩＴＥ                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     MOVE   "NVM0200I"       TO   PGID.
     MOVE   "FVM02001"       TO   FORMID.
     MOVE    SPACE           TO   DSP-PRO.
     WRITE   FVM02001.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FVM02001.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FVM02001"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     MOVE   "NVM0200I"       TO   PGID.
     MOVE   "FVM02001"       TO   FORMID.
     WRITE   FVM02001.
     INITIALIZE                   FVM02001.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*            商品名称マスタから画面セット
****************************************************************
 HMEIMS-SET-SEC              SECTION.
*
*更新前レコード退避
     MOVE     SPACE               TO   BEFORE-DATA.
     INITIALIZE                        BEFORE-DATA.
     MOVE     MEI-REC(1:170)      TO   BEFORE-DATA(1:170).
*
*項目セット
*  商品ＣＤ
     MOVE     MEI-F011       TO        SHOCD.
*  品単1
     MOVE     MEI-F0121      TO        TAN1.
*  品単2
     MOVE     MEI-F0122      TO        TAN2.
*  品単3
     MOVE     MEI-F0123      TO        TAN3.
*  商品名1
     MOVE     MEI-F021       TO        SHON1.
*  商品名2
     MOVE     MEI-F022       TO        SHON2.
*  商品カナ名1
     MOVE     MEI-F031       TO        KANA1.
*  商品カナ名2
     MOVE     MEI-F032       TO        KANA2.
*  入数
     MOVE     MEI-F07        TO        IRESU.
*  仕入単価
     MOVE     MEI-F041       TO        GENKA.
*  基準原価（卸）
     MOVE     MEI-F042       TO        GENTAN.
*  基準売価
     MOVE     MEI-F043       TO        URITAN.
*  名称変更区分
*    MOVE     MEI-F93        TO        KUBUN1.
*  自動発注区分
*    MOVE     MEI-F92        TO        HATKBN.
*  ユポラベル区分
*    MOVE     MEI-F94        TO        KUBUN2.
*  主仕入先
     MOVE     MEI-F05        TO        SIRCD.
     MOVE     MEI-F05        TO        SHI-F01.
     READ     ZSHIMS1
          INVALID
              MOVE   SPACE   TO        SIRNM
          NOT INVALID
              MOVE   SHI-F02 TO        SIRNM
     END-READ.
*  ＪＡＮＣＤ
     MOVE     MEI-F06        TO        JANCD.
*  定番区分
     MOVE     MEI-F95        TO        TEIBAN.
*  季節区分
*    MOVE     MEI-F96        TO        KISETU.
*    MOVE     SPACE          TO        JYO-REC.
*    INITIALIZE                        JYO-REC.
*    MOVE     81             TO        JYO-F01.
*    MOVE     MEI-F96        TO        JYO-F02(1:1).
*    READ     HJYOKEN
*         INVALID
*             MOVE  SPACE    TO        KINAME
*         NOT INVALID
*             MOVE  JYO-F14  TO        KINAME
*    END-READ.
*  廃盤区分
     MOVE     MEI-F08        TO        HAIBAN.
*  分類区分
     MOVE     MEI-F90        TO        BUNRUI.
     MOVE     SPACE          TO        JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     91             TO        JYO-F01.
     MOVE     MEI-F90        TO        JYO-F02.
     READ     HJYOKEN
          INVALID
              MOVE  SPACE    TO        BUNMEI
          NOT INVALID
              MOVE  JYO-F03  TO        BUNMEI
     END-READ.
*  管理区分
     MOVE     MEI-F91        TO        KANRI.
*  サカタ２０分類
     MOVE  MEI-F09           TO        BNR20.

     MOVE  10                TO        JYO-F01.
     MOVE  MEI-F09           TO        JYO-F02.
     PERFORM  HJYOKEN-RD-SEC.
     IF    FG-HJYOKEN-INV = 1
           MOVE  SPACE       TO        BNR20N
     ELSE
           MOVE JYO-F03      TO        BNR20N
     END-IF.
*  小売区分
     MOVE  MEI-F10           TO        KOURI.
*  物流束区分
     MOVE  MEI-F89           TO        BUTUR.
*　振替商品区分
     MOVE  MEI-FIL1          TO        FURIKA.
*↓2022/01/24
*  ストックＮＯ管理
     MOVE  MEI-F97           TO        STNO.
*↑2022/01/24
*　登録日
     IF  ( MEI-F98   NUMERIC   )  AND
         ( MEI-F98   NOT =   0 )
           MOVE  MEI-F98           TO        WK-MEI-F98
           MOVE  WK-MEI-F98(1:4)   TO        ADDDAT(1:4)
           MOVE  "/"               TO        ADDDAT(5:1)
           MOVE  WK-MEI-F98(5:2)   TO        ADDDAT(6:2)
           MOVE  "/"               TO        ADDDAT(8:1)
           MOVE  WK-MEI-F98(7:2)   TO        ADDDAT(9:2)
     END-IF.
*　修正日
     IF  ( MEI-F99   NUMERIC   )  AND
         ( MEI-F99   NOT =   0 )
           MOVE  MEI-F99           TO        WK-MEI-F99
           MOVE  WK-MEI-F99(1:4)   TO        UPDDAT(1:4)
           MOVE  "/"               TO        UPDDAT(5:1)
           MOVE  WK-MEI-F99(5:2)   TO        UPDDAT(6:2)
           MOVE  "/"               TO        UPDDAT(8:1)
           MOVE  WK-MEI-F99(7:2)   TO        UPDDAT(9:2)
     END-IF.
*
 HMEIMS-SET-EXIT.
     EXIT.
****************************************************************
*            サブ商品名称マスタから画面セット
****************************************************************
 SUBMEIF-SET-SEC              SECTION.
*
*更新前レコード退避
     MOVE     SPACE               TO   BEFORE-DATA.
     INITIALIZE                        BEFORE-DATA.
*2020/05/27↓
*    MOVE     SUB-REC(1:210)      TO   BEFORE-DATA(1:210).
*    MOVE     SUB-REC(211:262)    TO   BEFORE-DATA(211:262).
*    MOVE     SUB-REC(711:40)     TO   BEFORE-DATA(473:40).
     MOVE     SUB-REC(1:220)      TO   BEFORE-DATA(1:220).
     MOVE     SUB-REC(221:252)    TO   BEFORE-DATA(221:252).
     MOVE     SUB-REC(711:40)     TO   BEFORE-DATA(473:40).
*2020/05/27↑
*
*項目セット
*  商品ＣＤ
     MOVE     SUB-F011       TO        SHOCD.
*  品単1
     MOVE     SUB-F0121      TO        TAN1.
*  品単2
     MOVE     SUB-F0122      TO        TAN2.
*  品単3
     MOVE     SUB-F0123      TO        TAN3.
*  Ｄ３６５商品ＣＤ
     MOVE     SUB-D02        TO        D365CD.
*  Ｄ３６５ＪＡＮＣＤ
     MOVE     SUB-D01        TO        D365JN.
*  商品名1
     MOVE     SUB-F021       TO        SHON1.
*  商品名2
     MOVE     SUB-F022       TO        SHON2.
*  商品カナ名1
     MOVE     SUB-F031       TO        KANA1.
*  商品カナ名2
     MOVE     SUB-F032       TO        KANA2.
*  入数
     MOVE     SUB-F07        TO        IRESU.
*  仕入単価
     MOVE     SUB-F041       TO        GENKA.
*  基準原価（卸）
     MOVE     SUB-F042       TO        GENTAN.
*  基準売価
     MOVE     SUB-F043       TO        URITAN.
*  名称変更区分
*    MOVE     SUB-F93        TO        KUBUN1.
*  自動発注区分
*    MOVE     SUB-F92        TO        HATKBN.
*  ユポラベル区分
*    MOVE     SUB-F94        TO        KUBUN2.
*  主仕入先
     MOVE     SUB-F05        TO        SIRCD.
     MOVE     SUB-F05        TO        SHI-F01.
     READ     ZSHIMS1
          INVALID
              MOVE   SPACE   TO        SIRNM
          NOT INVALID
              MOVE   SHI-F02 TO        SIRNM
     END-READ.
*  ＪＡＮＣＤ
     MOVE     SUB-F06        TO        JANCD.
*  定番区分
     MOVE     SUB-F95        TO        TEIBAN.
*  季節区分
*    MOVE     SUB-F96        TO        KISETU.
*    MOVE     SPACE          TO        JYO-REC.
*    INITIALIZE                        JYO-REC.
*    MOVE     81             TO        JYO-F01.
*    MOVE     SUB-F96        TO        JYO-F02(1:1).
*    READ     HJYOKEN
*         INVALID
*             MOVE  SPACE    TO        KINAME
*         NOT INVALID
*             MOVE  JYO-F14  TO        KINAME
*    END-READ.
*  廃盤区分
     MOVE     SUB-F08        TO        HAIBAN.
*  分類区分
     MOVE     SUB-F90        TO        BUNRUI.
     MOVE     SPACE          TO        JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE     91             TO        JYO-F01.
     MOVE     SUB-F90        TO        JYO-F02.
     READ     HJYOKEN
          INVALID
              MOVE  SPACE    TO        BUNMEI
          NOT INVALID
              MOVE  JYO-F03  TO        BUNMEI
     END-READ.
*  管理区分
     MOVE     SUB-F91        TO        KANRI.
*  サカタ２０分類
     MOVE  SUB-F09           TO        BNR20.

     MOVE     10             TO        JYO-F01.
     MOVE     SUB-F09        TO        JYO-F02.
     PERFORM  HJYOKEN-RD-SEC.
     IF       FG-HJYOKEN-INV  =    1
              MOVE  SPACE    TO        BNR20N
     ELSE
              MOVE JYO-F03   TO        BNR20N
     END-IF.
*  小売区分
     MOVE     SUB-F10        TO        KOURI.
*  物流束区分
     MOVE     SUB-F89        TO        BUTUR.
*　振替商品区分
     MOVE     SUB-FIL1       TO        FURIKA.
*2020/05/27↓
*　オーダー区分
     MOVE     SUB-D03        TO        ODKBN.
*2020/05/27↑
*
*↓2022/01/24
*　ストックＮＯ管理区分
     MOVE     SUB-F97        TO        STNO.
*↑2022/01/24
*
*　登録担当者
     MOVE     SUB-D93        TO        ADDTAN.
*　登録日
     IF  ( SUB-D94   NUMERIC   )  AND
         ( SUB-D94   NOT =   0 )
           MOVE      SUB-D94(1:4)     TO        ADDDAT(1:4)
           MOVE      "/"              TO        ADDDAT(5:1)
           MOVE      SUB-D94(5:2)     TO        ADDDAT(6:2)
           MOVE      "/"              TO        ADDDAT(8:1)
           MOVE      SUB-D94(7:2)     TO        ADDDAT(9:2)
     END-IF.
*　修正担当者
     MOVE     SUB-D97        TO        UPDTAN.
*　修正日
     IF  ( SUB-D98   NUMERIC   )  AND
         ( SUB-D98   NOT =   0 )
           MOVE     SUB-D98(1:4)      TO        UPDDAT(1:4)
           MOVE     "/"               TO        UPDDAT(5:1)
           MOVE     SUB-D98(5:2)      TO        UPDDAT(6:2)
           MOVE     "/"               TO        UPDDAT(8:1)
           MOVE     SUB-D98(7:2)      TO        UPDDAT(9:2)
     END-IF.
*
 SUBMEIF-SET-EXIT.
     EXIT.
****************************************************************
*            画面から　商品名称マスタ　　　セット
*            　　　　　サブ商品名称マスタ　セット
****************************************************************
 DSP-TO-FILE-SEC           SECTION.
*  商品ＣＤ
     MOVE     SHOCD          TO        MEI-F011   SUB-F011.
*  品単1
     MOVE     TAN1           TO        MEI-F0121  SUB-F0121.
*  品単2
     MOVE     TAN2           TO        MEI-F0122  SUB-F0122.
*  品単3
     MOVE     TAN3           TO        MEI-F0123  SUB-F0123.
*  Ｄ３６５商品ＣＤ
     MOVE     D365CD         TO                   SUB-D02.
*  Ｄ３６５ＪＡＮＣＤ
     MOVE     D365JN         TO                   SUB-D01.
*  商品名1
     MOVE     SHON1          TO        MEI-F021   SUB-F021.
*  商品名2
     MOVE     SHON2          TO        MEI-F022   SUB-F022.
*  商品カナ名1
     MOVE     KANA1          TO        MEI-F031   SUB-F031.
*  商品カナ名2
     MOVE     KANA2          TO        MEI-F032   SUB-F032.
*  入数
     MOVE     IRESU          TO        MEI-F07    SUB-F07.
*  仕入単価
     MOVE     GENKA          TO        MEI-F041   SUB-F041.
*  基準原価（卸）
     MOVE     GENTAN         TO        MEI-F042   SUB-F042.
*  基準売価
     MOVE     URITAN         TO        MEI-F043   SUB-F043.
*  主仕入先ＣＤ
     MOVE     SIRCD          TO        MEI-F05    SUB-F05.
*  ＪＡＮＣＤ
     MOVE     JANCD          TO        MEI-F06    SUB-F06.
*  名称変更区分
*    MOVE     KUBUN1         TO        MEI-F93    SUB-F93.
*  自動発注区分
*    MOVE     HATKBN         TO        MEI-F92    SUB-F92.
*  ユポラベル区分
*    MOVE     KUBUN2         TO        MEI-F94    SUB-F94.
*  季節区分
*    MOVE     KISETU         TO        MEI-F96    SUB-F96.
*  定番区分
     MOVE     TEIBAN         TO        MEI-F95    SUB-F95.
*  廃盤区分
     MOVE     HAIBAN         TO        MEI-F08    SUB-F08.
*  分類区分
     MOVE     BUNRUI         TO        MEI-F90    SUB-F90.
*　管理区分
     MOVE     KANRI          TO        MEI-F91    SUB-F91.
*　サカタ20分類
     MOVE     BNR20          TO        MEI-F09    SUB-F09.
*  小売連携区分
     MOVE     KOURI          TO        MEI-F10    SUB-F10.
*  物流束区分
     MOVE     BUTUR          TO        MEI-F89    SUB-F89.
*  振替商品区分
     MOVE     FURIKA         TO        MEI-FIL1   SUB-FIL1.
*2020/05/27↓
*  オーダー区分
     MOVE     ODKBN          TO                   SUB-D03.
*2020/05/27↑
*
*↓2022/01/24
*　ストックＮＯ管理区分
     MOVE     STNO           TO        MEI-F97    SUB-F97.
*↑2022/01/24
*
*  更新日
     MOVE     SYS-DATE       TO        MEI-F99    SUB-F99.
*　更新担当者部門ＣＤ
     MOVE     PARA-BUMONCD   TO                   SUB-D96.
*　更新担当者ＣＤ
     MOVE     PARA-TANCD     TO                   SUB-D97.
*　更新日付
     MOVE     SYS-DATE       TO                   SUB-D98.
*　更新時刻
     MOVE     WK-TIME(1:6)   TO                   SUB-D99.
*
 DSP-TO-FILE-EXIT.
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
*-   ACCEPT    WK-TIME          FROM   TIME.
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
                   MOVE     "04"           TO    MSL-F01
                   MOVE     PARA-BUMONCD   TO    MSL-F02
                   MOVE     PARA-TANCD     TO    MSL-F03
                   MOVE     SYORI          TO    MSL-F04
                   MOVE     DATE-AREA      TO    MSL-F05
                   MOVE     WK-TIME(1:6)   TO    MSL-F06
                   MOVE     0              TO    MSL-F07
                   MOVE     DATA-TAIHI     TO    MSL-F08
                   PERFORM  BEFORE-SEC
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
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > SYORI)        OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
          MOVE      "04"            TO  MSL-F01
          MOVE      PARA-BUMONCD    TO  MSL-F02
          MOVE      PARA-TANCD      TO  MSL-F03
          MOVE      SYORI           TO  MSL-F04
          MOVE      DATE-AREA       TO  MSL-F05
          MOVE      WK-TIME(1:6)    TO  MSL-F06
          MOVE      SEQ             TO  MSL-F07
          MOVE      DATA-TAIHI      TO  MSL-F08
          PERFORM   BEFORE-SEC
          WRITE     MSL-REC
          GO        TO              MSTLOGF-WRITE-EXIT
     ELSE
          COMPUTE   SEQ =  MSL-F07  +  1
          GO        TO              MSTLOG-WRITE-010
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