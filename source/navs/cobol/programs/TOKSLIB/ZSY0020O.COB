***************************************************************
*                                                             *
*                  商品コード照会                             *
*                           Z S Y 0 0 2 0 O                   *
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               ZSY0020O.
 AUTHOR.                   K.A.
 DATE-WRITTEN.             93/04/30.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-6500.
 OBJECT-COMPUTER.          K-6500.
 SPECIAL-NAMES.
     STATION     IS        STA
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*商品名称マスタ（商品コード）
     SELECT      HMEIMS1   ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MEI1-F01
                           FILE      STATUS    MEI1-ST   MEI1-ST1.
*商品名称マスタ（商品名カナ）
     SELECT      HMEIMS4   ASSIGN    TO        DA-01-VI-MEIMS4
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MEI4-F03
                                     WITH      DUPLICATES
                           POSITIONING POINTER MEI4-POS
                           FILE      STATUS    MEI4-ST   MEI4-ST1.
*商品名称マスタ（ＪＡＮコード）
     SELECT      HMEIMS3   ASSIGN    TO        DA-01-VI-MEIMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MEI3-F06
                                     WITH      DUPLICATES
                           POSITIONING POINTER MEI3-POS
                           FILE      STATUS    MEI3-ST   MEI3-ST1.
*仕入先マスタ
     SELECT      ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SHI-F01
                           FILE      STATUS    SHI-ST    SHI-ST1.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST    JYO-ST1.
*画面ファイル*
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
                           SYMBOLIC  DESTINATION        "DSP"
                           DESTINATION-1       DSP-WS
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING  MODE    DSP-PRO
                           UNIT      CONTROL   DSP-UNIT
                           SELECTED  FUNCTION  DSP-FNC
                           FILE      STATUS    DSP-ST    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*商品名称マスタ
 FD  HMEIMS1
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI1      AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS4
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI4      AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS3
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI3      AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSHIMS    OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        ZSY0020  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*
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
                     NC"無効ＰＦキーです".
     03  MSG02             PIC N(20) VALUE
                     NC"対象データ　なし".
     03  MSG03             PIC N(20) VALUE
                     NC"検索条件を入力して下さい".
     03  MSG04             PIC N(20) VALUE
                     NC"検索条件が重複しています".
     03  MSG05             PIC N(20) VALUE
                     NC"前頁はありません".
     03  MSG06             PIC N(20) VALUE
                     NC"次頁はありません".
 01  MSG-AREAR             REDEFINES      MSG-AREA.
     03  MSGIX             PIC N(20)      OCCURS   6.
*
 01  PFKEY-MSG.
     03  PMSG01            PIC N(30) VALUE
           NC"_終了".
     03  PMSG02            PIC N(30) VALUE
           NC"_取消　_終了".
     03  PMSG03            PIC N(30) VALUE
           NC"_取消　_終了　_前頁".
     03  PMSG04            PIC N(30) VALUE
           NC"_取消　_終了　_次頁".
     03  PMSG05            PIC N(30) VALUE
           NC"_取消　_終了　_前頁　_次頁".
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  SYORI-FLG         PIC 9(01) VALUE     ZERO.
     03  SYORI-GROUP       PIC X(04).
     03  START-KEY         PIC X(30) VALUE     HIGH-VALUE.
     03  START-POS         PIC 9(09) COMP VALUE 0.
     03  END-KEY           PIC X(30) VALUE     LOW-VALUE.
     03  END-POS           PIC 9(09) COMP VALUE 0.
     03  WK-MEI-KEY        PIC X(30).
     03  WK-MEI-POS        PIC 9(09) COMP.
     03  WK-HEAD-KEY       PIC X(30).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  IX01              PIC 9(02) VALUE     ZERO.
     03  IX02              PIC 9(02) VALUE     ZERO.
     03  ERR-NO            PIC 9(02) VALUE     ZERO.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  MEI1-ST           PIC X(02).
     03  MEI1-ST1          PIC X(04).
     03  MEI4-POS          PIC 9(09) COMP VALUE 0.
     03  MEI4-ST           PIC X(02).
     03  MEI4-ST1          PIC X(04).
     03  MEI3-POS          PIC 9(09) COMP VALUE 0.
     03  MEI3-ST           PIC X(02).
     03  MEI3-ST1          PIC X(04).
     03  SHI-ST            PIC X(02).
     03  SHI-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  JYO-ST1           PIC X(04).
*
     03  FILE-ERR1         PIC N(20) VALUE
                     NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(20) VALUE
                     NC"商品名称マスタ（商品コード）異常！".
     03  FILE-ERR3         PIC N(20) VALUE
                     NC"商品名称マスタ（商品カナ名）異常！".
     03  FILE-ERR4         PIC N(20) VALUE
                     NC"商品名称マスタ（ＪＡＮコード）異常！".
     03  FILE-ERR5         PIC N(20) VALUE
                     NC"仕入先マスタ異常！".
     03  FILE-ERR6         PIC N(20) VALUE
                     NC"条件ファイル異常！".
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>-*
 01  SYS-DATE                PIC  9(06).
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>-*
 01  SYS-DATE2               PIC  9(08).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*             ファイルエラー処理
******************************************************************
 DECLARATIVES.
 MEI1-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HMEIMS1.
     DISPLAY       MEI1-ST   UPON      STA.
**** DISPLAY       MEI1-ST1  UPON      STA.
     DISPLAY       FILE-ERR2 UPON      STA.
**** ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI4-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HMEIMS4.
     DISPLAY       MEI4-ST   UPON      STA.
**** DISPLAY       MEI4-ST1  UPON      STA.
     DISPLAY       FILE-ERR3 UPON      STA.
**** ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI3-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HMEIMS3.
     DISPLAY       MEI3-ST   UPON      STA.
**** DISPLAY       MEI3-ST1  UPON      STA.
     DISPLAY       FILE-ERR4 UPON      STA.
**** ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 SHI-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZSHIMS.
     DISPLAY       SHI-ST    UPON      STA.
**** DISPLAY       SHI-ST1   UPON      STA.
     DISPLAY       FILE-ERR5 UPON      STA.
**** ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 JYO-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HJYOKEN.
     DISPLAY       JYO-ST    UPON      STA.
     DISPLAY       FILE-ERR6 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 DSP-ERR                SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
**** DISPLAY     DSP-ST1   UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
**** ACCEPT      IN-DATA   FROM      STA.
     MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*             メイン
******************************************************************
 SHORI-SEC              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC       UNTIL     END-FLG   =   9.
     PERFORM       END-SEC.
     STOP          RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*             初期処理
******************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HMEIMS1
                        HMEIMS4
                        HMEIMS3
                        ZSHIMS
                        HJYOKEN
              I-O       DSPFILE.
*
     ACCEPT   SYS-DATE      FROM  DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE2.
     ACCEPT    SYS-TIME    FROM  TIME.
*画面日付・時刻編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
*
     PERFORM            DSP-INIT-SEC.
     MOVE    "HEAD"               TO   SYORI-GROUP.
 INIT-EXIT.
     EXIT.
******************************************************************
*             初期画面　表示
******************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE     SPACE               TO   ZSY0020.
     MOVE     SPACE               TO   DSP-AREA.
     MOVE    "ZSY0020"            TO   DSP-FMT.
     MOVE    "SCREEN"             TO   DSP-GRP.
     MOVE    "CL"                 TO   DSP-PRO.
     WRITE    ZSY0020.
 DSP-INIT-EXIT.
     EXIT.
******************************************************************
*             主処理
******************************************************************
 MAIN-SEC               SECTION.
     MOVE     ZERO                TO   ERR-NO.
     EVALUATE      SYORI-GROUP
       WHEN       "HEAD"
              PERFORM        HEAD-SEC
       WHEN       "BODY"
              PERFORM        BODY-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
******************************************************************
*             ヘッド処理
******************************************************************
 HEAD-SEC               SECTION.
*ヘッド部入力
     MOVE     PMSG01              TO   MO002.
     MOVE    "SCREEN"             TO   DSP-GRP.
*NA* MOVE    "MO002"              TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     MOVE    "HEAD"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   HEAD-EXIT
     WHEN         "E000"
              CONTINUE
     WHEN          OTHER
              MOVE   1            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   HEAD-EXIT
     END-EVALUATE.
*検索条件 CHECK
     MOVE    "M"                  TO   EDIT-OPTION  OF  HI001.
     MOVE    "M"                  TO   EDIT-OPTION  OF  HI002.
     MOVE    "M"                  TO   EDIT-OPTION  OF  HI003.
     MOVE    " "                  TO   EDIT-CURSOR  OF  HI001.
     MOVE    " "                  TO   EDIT-CURSOR  OF  HI002.
     MOVE    " "                  TO   EDIT-CURSOR  OF  HI003.
     IF       HI001               =    SPACE
     AND      HI002               =    SPACE
     AND      HI003               =    SPACE
              MOVE   3            TO   ERR-NO
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI001
     END-IF.
     MOVE     HIGH-VALUE          TO   START-KEY.
     MOVE     0                   TO   START-POS    END-POS.
     EVALUATE TRUE
     WHEN     HI001          NOT  =    SPACE
         MOVE  "1"                TO   SYORI-FLG
         MOVE  HI001              TO   END-KEY  WK-HEAD-KEY
         IF   HI002          NOT  =    SPACE
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI001
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI002
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI001
         END-IF
         IF   HI003          NOT  =    SPACE
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI001
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI003
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI001
         END-IF
     WHEN     HI002          NOT  =    SPACE
         MOVE  "2"                TO   SYORI-FLG
         MOVE  HI002              TO   END-KEY  WK-HEAD-KEY
         IF   HI003          NOT  =    SPACE
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI002
              MOVE   "R"          TO   EDIT-CURSOR  OF  HI003
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
         END-IF
     WHEN     HI003          NOT  =    SPACE
         MOVE  "3"                TO   SYORI-FLG
         MOVE  HI003              TO   END-KEY  WK-HEAD-KEY
     END-EVALUATE.
*
     IF       ERR-NO              =    ZERO
              PERFORM   NEXT-PAGE-SEC
              IF   ERR-NO         =    ZERO
                   MOVE LOW-VALUE TO   START-KEY
              ELSE
                   MOVE  2        TO   ERR-NO
              END-IF
     END-IF.
*NA* MOVE    "HEAD"               TO   DSP-GRP.
*NA* PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    0
              MOVE  "BODY"        TO   DSP-GRP   SYORI-GROUP
*NA********** PERFORM   DSP-WR-SEC
     END-IF.
 HEAD-EXIT.
     EXIT.
******************************************************************
*             ボディ処理
******************************************************************
 BODY-SEC               SECTION.
*ボディ部入力
     IF       START-KEY           =    LOW-VALUE
         IF   END-KEY             =    HIGH-VALUE
              MOVE  PMSG02        TO   MO002
         ELSE
              MOVE  PMSG04        TO   MO002
         END-IF
     ELSE
         IF   END-KEY             =    HIGH-VALUE
              MOVE  PMSG03        TO   MO002
         ELSE
              MOVE  PMSG05        TO   MO002
         END-IF
     END-IF.
     MOVE    "SCREEN"             TO   DSP-GRP.
*NA* MOVE    "MO002"              TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     MOVE    "BODY"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   BODY-EXIT
     WHEN         "F004"
*NA********** PERFORM   DSP-INIT-SEC
              MOVE     SPACE               TO   ZSY0020
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   BODY-EXIT
**** WHEN         "F006"
****          MOVE     "HEAD"     TO   SYORI-GROUP
****                              GO   TO   BODY-EXIT
     WHEN         "E000"
              CONTINUE
     WHEN         "F007"
              PERFORM   BEFORE-PAGE-SEC
     WHEN         "F008"
              PERFORM   NEXT-PAGE-SEC
     WHEN          OTHER
              MOVE   1            TO   ERR-NO
     END-EVALUATE.
*NA* IF       ERR-NO              =    0
*NA*          MOVE  "BODY"        TO   DSP-GRP
*NA*          PERFORM   DSP-WR-SEC
*NA* END-IF.
     PERFORM            ERR-DSP-SEC.
 BODY-EXIT.
     EXIT.
******************************************************************
*             前画面処理
******************************************************************
 BEFORE-PAGE-SEC        SECTION.
     IF       START-KEY           =    LOW-VALUE
              MOVE  5             TO   ERR-NO
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     START-KEY           TO   WK-MEI-KEY.
     MOVE     START-POS           TO   WK-MEI-POS.
     PERFORM            MEIMS-START-RVS.
     IF       INV-SW              =    0
     AND      START-KEY           =    WK-MEI-KEY
     AND      START-POS           =    WK-MEI-POS
         PERFORM        MEIMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
         MOVE  5                  TO   ERR-NO
         MOVE  LOW-VALUE          TO   START-KEY
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     PERFORM       VARYING   IX01      FROM      8    BY   -1
                   UNTIL     IX01           <    1
                   OR        INV-SW         =    1
                   OR        WK-HEAD-KEY    >    WK-MEI-KEY
         MOVE  WK-MEI-KEY         TO   START-KEY
         MOVE  WK-MEI-POS         TO   START-POS
         PERFORM        MEIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
     OR       WK-HEAD-KEY         >    WK-MEI-KEY
         MOVE  WK-HEAD-KEY        TO   WK-MEI-KEY
         MOVE  0                  TO   WK-MEI-POS
         MOVE  LOW-VALUE          TO   START-KEY
         MOVE  0                  TO   START-POS
         MOVE  ZERO               TO   INV-SW
         PERFORM        MEIMS-INIT-START
     ELSE
         MOVE  START-KEY          TO   WK-MEI-KEY
         MOVE  START-POS          TO   WK-MEI-POS
         PERFORM        MEIMS-START-SEC
     END-IF.
     MOVE     SPACE               TO   BODYG.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    8
                   OR        INV-SW         =    1
         PERFORM        BODY-SET-SEC
         MOVE  WK-MEI-KEY         TO   END-KEY
         MOVE  WK-MEI-POS         TO   END-POS
         PERFORM        MEIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
         MOVE  HIGH-VALUE         TO   END-KEY
     END-IF.
 BEFORE-PAGE-EXIT.
     EXIT.
******************************************************************
*             次画面処理
******************************************************************
 NEXT-PAGE-SEC          SECTION.
     IF       END-KEY             =    HIGH-VALUE
              MOVE  6             TO   ERR-NO
                                  GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     END-KEY             TO   WK-MEI-KEY.
     MOVE     END-POS             TO   WK-MEI-POS.
     IF       SYORI-GROUP         =   "HEAD"
         PERFORM        MEIMS-INIT-START
     ELSE
         PERFORM        MEIMS-START-SEC
     END-IF.
     IF       INV-SW              =    0
     AND      END-KEY             =    WK-MEI-KEY
     AND      END-POS             =    WK-MEI-POS
     AND      SYORI-GROUP         =   "BODY"
         PERFORM        MEIMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
              MOVE  6             TO   ERR-NO
                                  GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     WK-MEI-KEY          TO   START-KEY.
     MOVE     WK-MEI-POS          TO   START-POS.
     MOVE     SPACE               TO   BODYG.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    8
                   OR        INV-SW         =    1
         PERFORM        BODY-SET-SEC
         MOVE  WK-MEI-KEY         TO   END-KEY
         MOVE  WK-MEI-POS         TO   END-POS
         PERFORM        MEIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
         MOVE  HIGH-VALUE         TO   END-KEY
     END-IF.
 NEXT-PAGE-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（降順）
******************************************************************
 MEIMS-START-RVS        SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-MEI-KEY             TO   MEI1-F01
         START    HMEIMS1        KEY  <=   MEI1-F01
                                 WITH      REVERSED  ORDER
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-MEI-KEY             TO   MEI4-F03
         MOVE  WK-MEI-POS             TO   MEI4-POS
         START    HMEIMS4
             POSITIONING    POINTER   IS   MEI4-POS
                                 KEY  =    MEI4-F03
                                 WITH      REVERSED  ORDER
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     3
         MOVE  WK-MEI-KEY             TO   MEI3-F06
         MOVE  WK-MEI-POS             TO   MEI3-POS
         START    HMEIMS3
             POSITIONING    POINTER   IS   MEI3-POS
                                 KEY  =    MEI3-F06
                                 WITH      REVERSED  ORDER
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
     IF       INV-SW                  =    0
         PERFORM       MEIMS-READ-SEC
     END-IF.
 MEIMS-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（昇順）
******************************************************************
 MEIMS-START-SEC        SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-MEI-KEY             TO   MEI1-F01
         START    HMEIMS1        KEY  >=   MEI1-F01
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-MEI-KEY             TO   MEI4-F03
         MOVE  WK-MEI-POS             TO   MEI4-POS
         START    HMEIMS4
             POSITIONING    POINTER   IS   MEI4-POS
                                 KEY  =    MEI4-F03
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     3
         MOVE  WK-MEI-KEY             TO   MEI3-F06
         MOVE  WK-MEI-POS             TO   MEI3-POS
         START    HMEIMS3
             POSITIONING    POINTER   IS   MEI3-POS
                                 KEY  =    MEI3-F06
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
     IF       INV-SW                  =    0
         PERFORM       MEIMS-READ-SEC
     END-IF.
 MEIMS-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け
******************************************************************
 MEIMS-INIT-START       SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-MEI-KEY             TO   MEI1-F01
         START    HMEIMS1        KEY  >=   MEI1-F01
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-MEI-KEY             TO   MEI4-F03
         MOVE  0                      TO   MEI4-POS
         START    HMEIMS4        KEY  >=   MEI4-F03
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     3
         MOVE  WK-MEI-KEY             TO   MEI3-F06
         MOVE  0                      TO   MEI3-POS
         START    HMEIMS3        KEY  >=   MEI3-F06
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
     IF       INV-SW                  =    0
         PERFORM       MEIMS-READ-SEC
     END-IF.
 MEIMS-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ読込み
******************************************************************
 MEIMS-READ-SEC         SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         READ     HMEIMS1        NEXT
             AT   END
                  MOVE  1             TO   INV-SW
             NOT  AT   END
                  MOVE  MEI1-F01      TO   WK-MEI-KEY
                  MOVE  0             TO   WK-MEI-POS
                  MOVE  MEI1-REC      TO   MEI-REC
         END-READ
     WHEN     2
         READ     HMEIMS4        NEXT
             AT   END
                  MOVE  1             TO   INV-SW
             NOT  AT   END
                  MOVE  MEI4-F03      TO   WK-MEI-KEY
                  MOVE  MEI4-POS      TO   WK-MEI-POS
                  MOVE  MEI4-REC      TO   MEI-REC
         END-READ
     WHEN     3
         READ     HMEIMS3        NEXT
             AT   END
                  MOVE  1             TO   INV-SW
             NOT  AT   END
                  MOVE  MEI3-F06      TO   WK-MEI-KEY
                  MOVE  MEI3-POS      TO   WK-MEI-POS
                  MOVE  MEI3-REC      TO   MEI-REC
         END-READ
     END-EVALUATE.
 MEIMS-READ-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.
     MOVE     MEI-F011           TO   BO001 (IX01).
     STRING   MEI-F012 (1:5)     "-"
              MEI-F012 (6:2)     "-"
              MEI-F012 (8:1)     "-"
              DELIMITED BY  SIZE INTO BO002 (IX01).
     MOVE     MEI-F021           TO   BO003 (IX01).
     MOVE     MEI-F022           TO   BO004 (IX01).
     MOVE     MEI-F05            TO   BO005 (IX01)    SHI-F01.
     READ     ZSHIMS
         INVALID
              MOVE  ALL NC"＊"   TO   BO006 (IX01)
         NOT  INVALID
              MOVE  SHI-F02      TO   BO006 (IX01)
     END-READ.
 BODY-SET-EXIT.
     EXIT.
****************************************************************
*             エラー処理
****************************************************************
 ERR-DSP-SEC            SECTION.
     IF       ERR-NO              =    ZERO
              MOVE  SPACE              TO   MO001
     ELSE
              MOVE  MSGIX (ERR-NO)     TO   MO001
     END-IF.
*NA* MOVE    "MO001"                   TO   DSP-GRP.
*NA* PERFORM            DSP-WR-SEC.
 ERR-DSP-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE         HMEIMS1
                   HMEIMS4
                   HMEIMS3
                   ZSHIMS
                   HJYOKEN
                   DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*             画面　　ＲＥＡＤ
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE    "NE"                 TO   DSP-PRO.
     READ     DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*             画面    ＷＲＩＴＥ
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     MOVE     SPACE               TO   DSP-PRO.
     WRITE    ZSY0020.
 DSP-WR-EXIT.
     EXIT.
