# ZSY0010O

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZSY0010O.COB`

## ソースコード

```cobol
***************************************************************
*                                                             *
*                  在庫照会                                   *
*                           Z S Y 0 0 1 0 O                   *
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               ZSY0010O.
 AUTHOR.                   K.A.
 DATE-WRITTEN.             93/05/06.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-6500.
 OBJECT-COMPUTER.          K-6500.
 SPECIAL-NAMES.
     STATION     IS        STA.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*商品在庫マスタ（商品コード・倉庫）
     SELECT      ZZAIMS2   ASSIGN    TO        DA-01-VI-ZZAIMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       ZAI2-F02
                                               ZAI2-F01
                                               ZAI2-F03
                           FILE      STATUS    ZAI2-ST   ZAI2-ST1.
*商品在庫マスタ（カナ名称・倉庫・商品コード）
     SELECT      ZZAIMS3   ASSIGN    TO        DA-01-VI-ZZAIMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       ZAI3-F04
                                               ZAI3-F02
                                               ZAI3-F01
                                               ZAI3-F03
                           FILE      STATUS    ZAI3-ST   ZAI3-ST1.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F01
                           FILE      STATUS    MEI-ST    MEI-ST1.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYOKEN-F01
                                               JYOKEN-F02
                           FILE      STATUS    JYKN-ST   JYKN-ST1.
*倉庫マスタ
     SELECT      ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST    SOK-ST1.
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
*商品在庫マスタ
 FD  ZZAIMS2
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZZAIMS    OF        XFDLIB
     JOINING     ZAI2      AS        PREFIX.
*商品在庫マスタ
 FD  ZZAIMS3
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZZAIMS    OF        XFDLIB
     JOINING     ZAI3      AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYOKEN    AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
**** BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        ZSY0010  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
     COPY        ZZAIMS    OF        XFDLIB
     JOINING     ZAI       AS        PREFIX.
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
                     NC"指定倉庫コードは扱えません".
     03  MSG06             PIC N(20) VALUE
                     NC"前頁はありません".
     03  MSG07             PIC N(20) VALUE
                     NC"次頁はありません".
     03  MSG08             PIC N(20) VALUE
                     NC"前レコードはありません".
     03  MSG09             PIC N(20) VALUE
                     NC"次レコードはありません".
     03  MSG10             PIC N(20) VALUE
                     NC"倉庫マスタに存在しません".
     03  MSG11             PIC N(20) VALUE
                     NC"指定されたレコードは存在しません".
 01  MSG-AREAR             REDEFINES      MSG-AREA.
     03  MSGIX             PIC N(20)      OCCURS   11.
*
 01  PFKEY-MSG.
     03  PMSG01            PIC N(30) VALUE
       NC"_終了".
     03  PMSG02            PIC N(30) VALUE
       NC"_取消　_終了　".
     03  PMSG02R.
       05  PMSG02A         PIC N(08).
       05  PMSG02IX        PIC N(01) OCCURS 22.
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  SYORI-FLG         PIC 9(01) VALUE     ZERO.
     03  SYORI-GROUP       PIC X(04).
     03  WK-E000-SOKO-CD         PIC X(02)  VALUE SPACE.
     03  WK-E000-TANA-BN         PIC X(06)  VALUE SPACE.
     03  E000-KEY.
       05    E000-KEY-KANA-NM    PIC X(15).
       05    E000-KEY-SYO-CD.
         07  E000-KEY-SYOHIN     PIC X(08).
         07  E000-KEY-HINTAN     PIC X(08).
       05    E000-KEY-SOKO-CD    PIC X(02).
     03  PF07-KEY                PIC X(39).
     03  PF08-KEY                PIC X(39).
     03  PF11-KEY.
       05    PF11-KEY-KANA-NM    PIC X(15).
       05    PF11-KEY-SYO-CD.
         07  PF11-KEY-SYOHIN     PIC X(08).
         07  PF11-KEY-HINTAN     PIC X(08).
       05    PF11-KEY-SOKO-CD    PIC X(02).
     03  PF12-KEY.
       05    PF12-KEY-KANA-NM    PIC X(15).
       05    PF12-KEY-SYO-CD.
         07  PF12-KEY-SYOHIN     PIC X(08).
         07  PF12-KEY-HINTAN     PIC X(08).
       05    PF12-KEY-SOKO-CD    PIC X(02).
     03  WK-ZAI-SOKO-CD          PIC X(02)  VALUE SPACE.
     03  WK-ZAI-TANA-BN          PIC X(06)  VALUE SPACE.
     03  WK-ZAI-KEY.
       05    WK-ZAI-KEY-KANA-NM  PIC X(15).
       05    WK-ZAI-KEY-SYO-CD.
         07  WK-ZAI-KEY-SYOHIN   PIC X(08).
         07  WK-ZAI-KEY-HINTAN   PIC X(08).
       05    WK-ZAI-KEY-SOKO-CD  PIC X(02).
       05    WK-ZAI-KEY-TANA-BN  PIC X(06).
     03  WK-HEAD-KEY.
       05    WK-HEAD-KEY-KANA-NM PIC X(15).
       05    WK-HEAD-KEY-SYO-CD.
         07  WK-HEAD-KEY-SYOHIN  PIC X(08).
         07  WK-HEAD-KEY-HINTAN  PIC X(08).
       05    WK-HEAD-KEY-SOKO-CD PIC X(02).
     03  SOKO-CD           PIC 9(02).
     03  WK-SOK-F02        PIC N(02).
     03  WK-MEI-F02G.
       05  WK-MEI-F02      PIC N(30).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  IX01              PIC 9(02) VALUE     ZERO.
     03  ZENGETU           PIC 9(02) VALUE     ZERO.
     03  MM                PIC 9(02) VALUE     ZERO.
     03  ERR-NO            PIC 9(02) VALUE     ZERO.
     03  WK-KETA           PIC 9(02) VALUE     ZERO.
 01  TOTAL-AREA.
     03  TTL-F06           PIC S9(11)V99.
     03  TTL-F07           PIC S9(11)V99.
     03  TTL-F10           PIC S9(11)V99.
     03  TTL-TBL-F152      PIC S9(11)V99.
     03  TTL-F11           PIC S9(11)V99.
     03  TTL-TBL-F153      PIC S9(11)V99.
     03  TTL-F12           PIC S9(11)V99.
     03  TTL-F13           PIC S9(11)V99.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  ZAI2-ST           PIC X(02).
     03  ZAI2-ST1          PIC X(04).
     03  ZAI3-ST           PIC X(02).
     03  ZAI3-ST1          PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  JYKN-ST           PIC X(02).
     03  JYKN-ST1          PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
*
     03  FILE-ERR1         PIC N(20) VALUE
                     NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(20) VALUE
                     NC"商品在庫マスタ異常！".
     03  FILE-ERR3         PIC N(20) VALUE
                     NC"商品在庫マスタ（カナ名称）異常！".
     03  FILE-ERR4         PIC N(20) VALUE
                     NC"商品名称マスタ異常！".
     03  FILE-ERR5         PIC N(10) VALUE
                     NC"条件ファイル異常！".
     03  FILE-ERR6         PIC N(10) VALUE
                     NC"倉庫マスタ異常！".
******************************************************************
 LINKAGE                SECTION.
******************************************************************
 01  LINK-AREA.
**** 03                    PIC X(02).
     03  LINK-WS-NAME      PIC X(08).
******************************************************************
 PROCEDURE              DIVISION       USING     LINK-AREA.
******************************************************************
*             ファイルエラー処理
******************************************************************
 DECLARATIVES.
 ZAI2-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZZAIMS2.
     DISPLAY       ZAI2-ST   UPON      STA.
     DISPLAY       ZAI2-ST1  UPON      STA.
     DISPLAY       FILE-ERR2 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 ZAI3-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZZAIMS3.
     DISPLAY       ZAI3-ST   UPON      STA.
     DISPLAY       ZAI3-ST1  UPON      STA.
     DISPLAY       FILE-ERR3 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HMEIMS.
     DISPLAY       MEI-ST    UPON      STA.
     DISPLAY       MEI-ST1   UPON      STA.
     DISPLAY       FILE-ERR4 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 JYKN-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HJYOKEN.
     DISPLAY       JYKN-ST   UPON      STA.
     DISPLAY       JYKN-ST1  UPON      STA.
     DISPLAY       FILE-ERR5 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 SOK-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZSOKMS.
     DISPLAY       SOK-ST    UPON      STA.
     DISPLAY       SOK-ST1   UPON      STA.
     DISPLAY       FILE-ERR6 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 DSP-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         DSPFILE.
     DISPLAY       DSP-ST    UPON      STA.
     DISPLAY       DSP-ST1   UPON      STA.
     DISPLAY       FILE-ERR1 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
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
     OPEN     INPUT     ZZAIMS2
                        ZZAIMS3
                        HMEIMS
                        HJYOKEN
                        ZSOKMS
              I-O       DSPFILE.
*    前月度
     MOVE     99                  TO   JYOKEN-F01.
     MOVE     "ZAI"               TO   JYOKEN-F02.
     READ     HJYOKEN
         INVALID
              DISPLAY "* HJYOKEN  INVALID KEY:"
                   JYOKEN-F01 "," JYOKEN-F02 "*" UPON STA
              MOVE  9             TO   END-FLG
                                  GO   TO   INIT-EXIT
         NOT  INVALID
              MOVE  JYOKEN-F04    TO   ZENGETU
              IF   ZENGETU        >    5
                   COMPUTE   MM   =    ZENGETU   -    5
              ELSE
                   COMPUTE   MM   =    ZENGETU   +    7
              END-IF
              ADD  -1             TO   MM
              IF   MM             =    0
                   MOVE  12            TO   MM
              END-IF
     END-READ.
*    倉庫コード
     MOVE     65                  TO   JYOKEN-F01.
     MOVE     LINK-WS-NAME        TO   JYOKEN-F02.
     READ     HJYOKEN
         INVALID
              DISPLAY "* HJYOKEN  INVALID KEY:"
                   JYOKEN-F01 "," JYOKEN-F02 "*" UPON STA
              MOVE  9             TO   END-FLG
                                  GO   TO   INIT-EXIT
         NOT  INVALID
              MOVE  JYOKEN-F04    TO   SOKO-CD
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
     MOVE     SPACE               TO   ZSY0010.
     IF       SOKO-CD             =    01
              CONTINUE
     ELSE
              MOVE  SOKO-CD       TO   HI001
     END-IF.
     MOVE    "ZSY0010"            TO   DSP-FMT.
     MOVE    "SCREEN"             TO   DSP-GRP.
     MOVE    "CL"                 TO   DSP-PRO.
     WRITE    ZSY0010.
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
     PERFORM            PFK-DSP-SEC.
     IF       SOKO-CD             =    1
              CONTINUE
     ELSE
              MOVE  "X"           TO   EDIT-STATUS  OF  HI001
     END-IF.
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
     IF       HI001          NOT  =    SPACE
              MOVE  HI001         TO   SOK-F01
              READ      ZSOKMS
                INVALID
                        IF   ERR-NO    =    ZERO
                             MOVE  10  TO   ERR-NO
                        END-IF
                        MOVE "R"  TO   EDIT-OPTION  OF  HI001
                        MOVE "C"  TO   EDIT-CURSOR  OF  HI001
                        MOVE NC"＊＊"  TO  SOK-F02
                NOT INVALID
                        MOVE SOK-F02   TO  WK-SOK-F02
              END-READ
     END-IF.
     MOVE     LOW-VALUE           TO   PF07-KEY  PF11-KEY.
     MOVE     SPACE               TO   PF08-KEY  PF12-KEY.
     EVALUATE TRUE
     WHEN   ( HI002               =    SPACE )  AND
            ( HI003               =    SPACE )
              IF   ERR-NO         =    ZERO
                   MOVE  3        TO   ERR-NO
              END-IF
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
     WHEN   ( HI002          NOT  =    SPACE )  AND
            ( HI003          NOT  =    SPACE )
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-OPTION  OF  HI002
              MOVE   "R"          TO   EDIT-OPTION  OF  HI003
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
     WHEN     HI002          NOT  =    SPACE
              MOVE  "1"                TO   SYORI-FLG
              MOVE  HI002              TO   PF12-KEY-SYO-CD
              IF    HI001         NOT  =   "  "
                    MOVE  HI001        TO   PF12-KEY-SOKO-CD
              END-IF
              MOVE  PF12-KEY           TO   WK-HEAD-KEY
     WHEN     HI003          NOT  =    SPACE
              PERFORM VARYING WK-KETA FROM 15 BY -1
                      UNTIL ( WK-KETA              =   1     ) OR
                            ( HI003(WK-KETA:1) NOT =   SPACE )
                      CONTINUE
              END-PERFORM
              MOVE  "2"                TO   SYORI-FLG
              MOVE  HI003              TO   PF12-KEY-KANA-NM
              IF    HI001         NOT  =   "  "
                    MOVE  HI001        TO   PF12-KEY-SOKO-CD
              END-IF
              MOVE  PF12-KEY           TO   WK-HEAD-KEY
     END-EVALUATE.
*
     IF       ERR-NO              =    ZERO
              PERFORM   NEXT-REC-SEC
              MOVE  LOW-VALUE     TO   PF11-KEY
              MOVE  E000-KEY      TO   WK-HEAD-KEY
              IF   ERR-NO         =    ZERO
*******************PERFORM   MEIMS-READ-SEC
                   MOVE  "BODY"   TO   SYORI-GROUP
              ELSE
                   IF   ERR-NO    =    9
                        MOVE  2        TO   ERR-NO
                   ELSE
                        MOVE  11       TO   ERR-NO
                        MOVE  E000-KEY TO   PF12-KEY
                        MOVE LOW-VALUE TO   E000-KEY
                        MOVE  "BODY"   TO   SYORI-GROUP
                   END-IF
              END-IF
     END-IF.
     MOVE    "HEAD"               TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    0
              MOVE  "BODY"        TO   DSP-GRP
              PERFORM   DSP-WR-SEC
     END-IF.
 HEAD-EXIT.
     EXIT.
******************************************************************
*             ボディ処理
******************************************************************
 BODY-SEC               SECTION.
*ボディ部入力
     PERFORM            PFK-DSP-SEC.
     MOVE    "BODY"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   BODY-EXIT
     WHEN         "F004"
              PERFORM   DSP-INIT-SEC
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   BODY-EXIT
**** WHEN         "F006"
****          MOVE     "HEAD"     TO   SYORI-GROUP
****                              GO   TO   BODY-EXIT
     WHEN         "E000"
              CONTINUE
     WHEN         "F011"
              PERFORM   BEFORE-REC-SEC
     WHEN         "F012"
              PERFORM   NEXT-REC-SEC
     WHEN         "F007"
              PERFORM   BEFORE-PAGE-SEC
     WHEN         "F008"
              PERFORM   NEXT-PAGE-SEC
     WHEN          OTHER
              MOVE   1            TO   ERR-NO
     END-EVALUATE.
     IF       ERR-NO              =    0
              MOVE  "BODY"        TO   DSP-GRP
              PERFORM   DSP-WR-SEC
     END-IF.
     PERFORM            ERR-DSP-SEC.
 BODY-EXIT.
     EXIT.
******************************************************************
*             前レコード処理
******************************************************************
 BEFORE-REC-SEC         SECTION.
*
     IF       PF11-KEY            =    LOW-VALUE
     OR       PF11-KEY            <    WK-HEAD-KEY
              MOVE  8             TO   ERR-NO
                                  GO   TO   BEFORE-REC-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     PF11-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-SEC.
*
     IF       INV-SW              =    1
     OR       WK-ZAI-KEY(1:33)    <    WK-HEAD-KEY
              MOVE  8             TO   ERR-NO
                                  GO   TO   BEFORE-REC-EXIT
     END-IF.
     MOVE     E000-KEY            TO   PF12-KEY.
     MOVE     WK-ZAI-KEY          TO   E000-KEY.
     MOVE     WK-ZAI-SOKO-CD      TO   WK-E000-SOKO-CD.
     MOVE     WK-ZAI-TANA-BN      TO   WK-E000-TANA-BN.
     MOVE     LOW-VALUE           TO   PF07-KEY.
     PERFORM            HEAD-SET-SEC.
     MOVE     SPACE               TO   BODYG.
     MOVE     1                   TO   IX01.
     IF       HI001               =   "  "
         PERFORM        TOTAL-ADD-SEC
         PERFORM        TOTAL-SET-SEC
         MOVE  ZERO               TO   INV-SW
         MOVE  E000-KEY           TO   WK-ZAI-KEY
         MOVE  WK-E000-SOKO-CD    TO   WK-ZAI-KEY-SOKO-CD
         MOVE  WK-E000-TANA-BN    TO   WK-ZAI-KEY-TANA-BN
         PERFORM        ZAIMS-START-SEC
     END-IF.
     PERFORM       VARYING   IX01      FROM      IX01 BY   1
                   UNTIL     IX01           >    7
                   OR        INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
              PERFORM   BODY-SET-SEC
              MOVE  WK-ZAI-KEY    TO   PF08-KEY
              MOVE  ZAI-F01       TO   PF08-KEY(32:2)
              PERFORM   ZAIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    0
     AND      E000-KEY            =    WK-ZAI-KEY(1:33)
              CONTINUE
     ELSE
              MOVE  HIGH-VALUE    TO   PF08-KEY
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     PERFORM            BEFORE-REC-SEARCH.
*
     IF       INV-SW              =    0
     AND      WK-ZAI-KEY(1:33)    >=   WK-HEAD-KEY
         MOVE  WK-ZAI-KEY         TO   PF11-KEY
     ELSE
         MOVE  LOW-VALUE          TO   PF11-KEY
     END-IF.
 BEFORE-REC-EXIT.
     EXIT.
******************************************************************
*             次レコード処理
******************************************************************
 NEXT-REC-SEC           SECTION.
     IF       PF12-KEY            =    HIGH-VALUE
              MOVE  9             TO   ERR-NO
                                  GO   TO   NEXT-REC-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     PF12-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-SEC.
     IF       INV-SW              =    1
              MOVE  9             TO   ERR-NO
                                  GO   TO   NEXT-REC-EXIT
     END-IF.
     MOVE     E000-KEY            TO   PF11-KEY.
     MOVE     WK-ZAI-KEY          TO   E000-KEY.
     MOVE     WK-ZAI-SOKO-CD      TO   WK-E000-SOKO-CD.
     MOVE     WK-ZAI-TANA-BN      TO   WK-E000-TANA-BN.
     MOVE     LOW-VALUE           TO   PF07-KEY.
     MOVE     HIGH-VALUE          TO   PF08-KEY.
     IF       SYORI-GROUP         =   "HEAD"
         IF ( SYORI-FLG           =    1
         AND  HI002               <    WK-ZAI-KEY-SYOHIN  )
         OR ( SYORI-FLG           =    2
         AND  HI003(1:WK-KETA)    <
                                  WK-ZAI-KEY-KANA-NM(1:WK-KETA) )
              MOVE  11            TO   ERR-NO
                                  GO   TO   NEXT-REC-EXIT
         END-IF
     END-IF.
     PERFORM            HEAD-SET-SEC.
     MOVE     SPACE               TO   BODYG.
     MOVE     1                   TO   IX01.
     IF       HI001               =   "  "
         PERFORM        TOTAL-ADD-SEC
         PERFORM        TOTAL-SET-SEC
         MOVE  ZERO               TO   INV-SW
         MOVE  E000-KEY           TO   WK-ZAI-KEY
         MOVE  WK-E000-SOKO-CD    TO   WK-ZAI-KEY-SOKO-CD
         MOVE  WK-E000-TANA-BN    TO   WK-ZAI-KEY-TANA-BN
         PERFORM        ZAIMS-START-SEC
     END-IF.
     PERFORM       VARYING   IX01      FROM      IX01 BY   1
                   UNTIL     IX01           >    7
                   OR        INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
              PERFORM   BODY-SET-SEC
              MOVE  WK-ZAI-KEY    TO   PF08-KEY
              MOVE  ZAI-F01       TO   PF08-KEY(32:2)
              PERFORM   ZAIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    0
     AND      E000-KEY            =    WK-ZAI-KEY(1:33)
              CONTINUE
     ELSE
              MOVE  HIGH-VALUE    TO   PF08-KEY
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     PERFORM            NEXT-REC-SEARCH.
     IF       INV-SW              =    0
         MOVE  WK-ZAI-KEY         TO   PF12-KEY
     ELSE
         MOVE  HIGH-VALUE         TO   PF12-KEY
     END-IF.
 NEXT-REC-EXIT.
     EXIT.
******************************************************************
*             前頁処理
******************************************************************
 BEFORE-PAGE-SEC        SECTION.
     IF       PF07-KEY            =    LOW-VALUE
              MOVE  6             TO   ERR-NO
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     PF07-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-RVS.
     IF       INV-SW              =    0
         PERFORM        ZAIMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
         MOVE  6                  TO   ERR-NO
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
*
     IF       HI001               =   "  "
         MOVE  6                  TO   IX01
     ELSE
         MOVE  7                  TO   IX01
     END-IF.
     PERFORM       VARYING   IX01      FROM      IX01 BY   -1
                   UNTIL     IX01           <    1
                   OR        INV-SW         =    1
                   OR        E000-KEY       >    WK-ZAI-KEY(1:33)
         MOVE  WK-ZAI-KEY         TO   PF07-KEY
         MOVE  ZAI-F01            TO   PF07-KEY(32:2)
         PERFORM        ZAIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
     OR       E000-KEY            >    WK-ZAI-KEY(1:33)
         MOVE  E000-KEY           TO   WK-ZAI-KEY
         MOVE  LOW-VALUE          TO   PF07-KEY
         MOVE  ZERO               TO   INV-SW
     ELSE
         MOVE  PF07-KEY           TO   WK-ZAI-KEY
     END-IF.
     PERFORM            ZAIMS-START-SEC.
     MOVE     SPACE               TO   BODYG.
     MOVE     1                   TO   IX01.
     IF       HI001               =   "  "
              PERFORM   TOTAL-SET-SEC
     END-IF.
     PERFORM       VARYING   IX01      FROM      IX01 BY   1
                   UNTIL     IX01           >    7
                   OR        INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
         PERFORM        BODY-SET-SEC
         MOVE  WK-ZAI-KEY         TO   PF08-KEY
         MOVE  ZAI-F01            TO   PF08-KEY(32:2)
         PERFORM        ZAIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    0
     AND      E000-KEY            =    WK-ZAI-KEY(1:33)
         CONTINUE
     ELSE
         MOVE  WK-ZAI-KEY         TO   PF08-KEY
     END-IF.
 BEFORE-PAGE-EXIT.
     EXIT.
******************************************************************
*             次頁処理
******************************************************************
 NEXT-PAGE-SEC          SECTION.
     IF       PF08-KEY            =    HIGH-VALUE
              MOVE  7             TO   ERR-NO
                                  GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     PF08-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-SEC.
     IF       INV-SW              =    0
         PERFORM        ZAIMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
              MOVE  7             TO   ERR-NO
                                  GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     WK-ZAI-KEY          TO   PF07-KEY.
     MOVE     ZAI-F01             TO   PF07-KEY(32:2).
     MOVE     SPACE               TO   BODYG.
     MOVE     1                   TO   IX01.
     IF       HI001               =   "  "
              PERFORM   TOTAL-SET-SEC
     END-IF.
     PERFORM       VARYING   IX01      FROM      IX01 BY   1
                   UNTIL     IX01           >    7
                   OR        INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
         PERFORM        BODY-SET-SEC
         MOVE  WK-ZAI-KEY         TO   PF08-KEY
         MOVE  ZAI-F01            TO   PF08-KEY(32:2)
         PERFORM        ZAIMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    0
     AND      E000-KEY            =    WK-ZAI-KEY(1:33)
         CONTINUE
     ELSE
         MOVE  HIGH-VALUE         TO   PF08-KEY
     END-IF.
 NEXT-PAGE-EXIT.
     EXIT.
******************************************************************
*             合計計算処理
******************************************************************
 TOTAL-ADD-SEC          SECTION.
     MOVE     ZERO                TO   TOTAL-AREA.
     PERFORM       UNTIL     INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
         ADD   ZAI-F06            TO   TTL-F06
         ADD   ZAI-F07            TO   TTL-F07
         ADD   ZAI-F10            TO   TTL-F10
         ADD   ZAI-F11            TO   TTL-F11
         ADD   ZAI-F12            TO   TTL-F12
         ADD   ZAI-F13            TO   TTL-F13
         IF   MM                  =    12
              ADD   ZAI-F152 (MM) TO   TTL-TBL-F152
              ADD   ZAI-F153 (MM) TO   TTL-TBL-F153
         ELSE
              ADD   ZAI-F162 (MM) TO   TTL-TBL-F152
              ADD   ZAI-F163 (MM) TO   TTL-TBL-F153
         END-IF
         PERFORM        ZAIMS-READ-SEC
     END-PERFORM.
 TOTAL-ADD-EXIT.
     EXIT.
******************************************************************
*             前レコード検索処理
******************************************************************
 BEFORE-REC-SEARCH      SECTION.
     MOVE     E000-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-RVS.
     IF       INV-SW              =    0
         PERFORM   UNTIL     INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
              PERFORM   ZAIMS-READ-SEC
         END-PERFORM
     END-IF.
 BEFORE-REC-SEARCH-EXIT.
     EXIT.
******************************************************************
*             次レコード検索処理
******************************************************************
 NEXT-REC-SEARCH        SECTION.
     MOVE     E000-KEY            TO   WK-ZAI-KEY.
     PERFORM            ZAIMS-START-SEC.
     IF       INV-SW              =    0
         PERFORM   UNTIL     INV-SW         =    1
                   OR        E000-KEY  NOT  =    WK-ZAI-KEY(1:33)
              PERFORM   ZAIMS-READ-SEC
         END-PERFORM
     END-IF.
 NEXT-REC-SEARCH-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（降順）
******************************************************************
 ZAIMS-START-RVS        SECTION.
     IF       SYORI-FLG           =    1
         PERFORM        ZAIMS2-START-RVS
     ELSE
         PERFORM        ZAIMS3-START-RVS
     END-IF.
 ZAIMS-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（降順）
******************************************************************
 ZAIMS2-START-RVS       SECTION.
     MOVE     WK-ZAI-KEY-SOKO-CD  TO   ZAI2-F01.
     MOVE     WK-ZAI-KEY-SYO-CD   TO   ZAI2-F02.
     MOVE     WK-ZAI-KEY-TANA-BN  TO   ZAI2-F03.
     START    ZZAIMS2        KEY  <=   ZAI2-F02
                                       ZAI2-F01
                                       ZAI2-F03
                             WITH      REVERSED  ORDER
         INVALID  KEY
              MOVE  1             TO   INV-SW
         NOT  INVALID   KEY
              PERFORM        ZAIMS2-READ-SEC
     END-START.
 ZAIMS2-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（降順）
******************************************************************
 ZAIMS3-START-RVS       SECTION.
     MOVE     WK-ZAI-KEY-SOKO-CD  TO   ZAI3-F01.
     MOVE     WK-ZAI-KEY-SYO-CD   TO   ZAI3-F02.
     MOVE     WK-ZAI-KEY-TANA-BN  TO   ZAI3-F03.
     MOVE     WK-ZAI-KEY-KANA-NM  TO   ZAI3-F04.
     START    ZZAIMS3        KEY  <=   ZAI3-F04
                                       ZAI3-F02
                                       ZAI3-F01
                                       ZAI3-F03
                             WITH      REVERSED  ORDER
         INVALID  KEY
              MOVE  1             TO   INV-SW
         NOT  INVALID   KEY
              PERFORM        ZAIMS3-READ-SEC
     END-START.
 ZAIMS3-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（昇順）
******************************************************************
 ZAIMS-START-SEC        SECTION.
     IF       SYORI-FLG           =    1
         PERFORM        ZAIMS2-START-SEC
     ELSE
         PERFORM        ZAIMS3-START-SEC
     END-IF.
 ZAIMS-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（昇順）
******************************************************************
 ZAIMS2-START-SEC       SECTION.
     MOVE     WK-ZAI-KEY-SOKO-CD  TO   ZAI2-F01.
     MOVE     WK-ZAI-KEY-SYO-CD   TO   ZAI2-F02.
     MOVE     WK-ZAI-KEY-TANA-BN  TO   ZAI2-F03.
     START    ZZAIMS2        KEY  >=   ZAI2-F02
                                       ZAI2-F01
                                       ZAI2-F03
         INVALID   KEY
              MOVE  1             TO   INV-SW
         NOT  INVALID   KEY
              PERFORM        ZAIMS2-READ-SEC
     END-START.
 ZAIMS2-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ位置付け（昇順）
******************************************************************
 ZAIMS3-START-SEC       SECTION.
     MOVE     WK-ZAI-KEY-SOKO-CD  TO   ZAI3-F01.
     MOVE     WK-ZAI-KEY-SYO-CD   TO   ZAI3-F02.
     MOVE     WK-ZAI-KEY-TANA-BN  TO   ZAI3-F03.
     MOVE     WK-ZAI-KEY-KANA-NM  TO   ZAI3-F04.
     START    ZZAIMS3        KEY  >=   ZAI3-F04
                                       ZAI3-F02
                                       ZAI3-F01
                                       ZAI3-F03
         INVALID   KEY
              MOVE  1             TO   INV-SW
         NOT  INVALID   KEY
              PERFORM        ZAIMS3-READ-SEC
     END-START.
 ZAIMS3-START-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ読込み
******************************************************************
 ZAIMS-READ-SEC         SECTION.
     IF       SYORI-FLG           =    1
         PERFORM        ZAIMS2-READ-SEC
     ELSE
         PERFORM        ZAIMS3-READ-SEC
     END-IF.
 ZAIMS-READ-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ読込み
******************************************************************
 ZAIMS2-READ-SEC        SECTION.
     READ     ZZAIMS2        NEXT
         AT   END
              MOVE  1             TO   INV-SW
         NOT  AT   END
              MOVE  SPACE         TO   WK-ZAI-KEY
              IF   HI001     NOT  =   "  "
                   IF   HI001     NOT  =    ZAI2-F01
                                  GO   TO   ZAIMS2-READ-SEC
                   END-IF
                   MOVE  ZAI2-F01      TO   WK-ZAI-KEY-SOKO-CD
              END-IF
              MOVE  ZAI2-F01      TO   WK-ZAI-SOKO-CD
              MOVE  ZAI2-F03      TO   WK-ZAI-TANA-BN
              MOVE  ZAI2-F02      TO   WK-ZAI-KEY-SYO-CD
              MOVE  ZAI2-F03      TO   WK-ZAI-KEY-TANA-BN
              IF   SYORI-FLG      =    2
                   MOVE  ZAI2-F04      TO   WK-ZAI-KEY-KANA-NM
              END-IF
              MOVE  ZAI2-REC      TO   ZAI-REC
     END-READ.
 ZAIMS2-READ-EXIT.
     EXIT.
******************************************************************
*             商品在庫マスタ読込み
******************************************************************
 ZAIMS3-READ-SEC        SECTION.
     READ     ZZAIMS3        NEXT
         AT   END
              MOVE  1             TO   INV-SW
         NOT  AT   END
              MOVE  SPACE         TO   WK-ZAI-KEY
              IF   HI001     NOT  =   "  "
                   IF   HI001     NOT  =    ZAI3-F01
                                  GO   TO   ZAIMS3-READ-SEC
                   END-IF
                   MOVE  ZAI3-F01      TO   WK-ZAI-KEY-SOKO-CD
              END-IF
              MOVE  ZAI3-F01      TO   WK-ZAI-SOKO-CD
              MOVE  ZAI3-F03      TO   WK-ZAI-TANA-BN
              MOVE  ZAI3-F02      TO   WK-ZAI-KEY-SYO-CD
              MOVE  ZAI3-F03      TO   WK-ZAI-KEY-TANA-BN
              IF   SYORI-FLG      =    2
                   MOVE  ZAI3-F04      TO   WK-ZAI-KEY-KANA-NM
              END-IF
              MOVE  ZAI3-REC      TO   ZAI-REC
     END-READ.
 ZAIMS3-READ-EXIT.
     EXIT.
******************************************************************
*             ヘッドセット処理
******************************************************************
 HEAD-SET-SEC           SECTION.
     MOVE     ZAI-F02             TO   HI002.
     MOVE     ZAI-F04             TO   HI003.
     PERFORM            MEIMS-READ-SEC.
     MOVE    "HEAD"               TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
 HEAD-SET-EXIT.
     EXIT.
******************************************************************
*             商品名称マスタ読込み
******************************************************************
 MEIMS-READ-SEC         SECTION.
     MOVE     ZAI-F02             TO   MEI-F01.
     READ     HMEIMS
         INVALID
              MOVE  ALL NC"＊"    TO   HO001
         NOT  INVALID
              MOVE  MEI-F02       TO   WK-MEI-F02G
              MOVE  WK-MEI-F02    TO   HO001
     END-READ.
 MEIMS-READ-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理（合計）
******************************************************************
 TOTAL-SET-SEC          SECTION.
     MOVE     NC"合計"            TO   BO001 (IX01).
**** MOVE                         TO   BO002 (IX01).
**** MOVE                         TO   BO003 (IX01).
**** MOVE                         TO   BO004 (IX01).
**** MOVE                         TO   BO005 (IX01).
     MOVE     TTL-F06             TO   BO006 (IX01).
     MOVE     TTL-F07             TO   BO007 (IX01).
     MOVE     TTL-F10             TO   BO008 (IX01).
     MOVE     TTL-F11             TO   BO010 (IX01).
     MOVE     TTL-F12             TO   BO012 (IX01).
     MOVE     TTL-F13             TO   BO013 (IX01).
     MOVE     TTL-TBL-F152        TO   BO009 (IX01).
     MOVE     TTL-TBL-F153        TO   BO011 (IX01).
     ADD      1                   TO          IX01 .
 TOTAL-SET-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.
     IF       HI001               =   "  "
              MOVE  ZAI-F01       TO   SOK-F01
              READ      ZSOKMS
                INVALID
                   MOVE  NC"＊＊" TO   BO001 (IX01)
                NOT  INVALID
                   MOVE  SOK-F02  TO   BO001 (IX01)
              END-READ
     ELSE
              MOVE  WK-SOK-F02    TO   BO001 (IX01)
     END-IF.
     MOVE     ZAI-F03             TO   BO002 (IX01).
     MOVE     ZAI-F022 (1:5)      TO   BO003 (IX01).
     MOVE     ZAI-F022 (6:2)      TO   BO004 (IX01).
     MOVE     ZAI-F022 (8:1)      TO   BO005 (IX01).
     MOVE     ZAI-F06             TO   BO006 (IX01).
     MOVE     ZAI-F07             TO   BO007 (IX01).
     MOVE     ZAI-F10             TO   BO008 (IX01).
     MOVE     ZAI-F11             TO   BO010 (IX01).
     MOVE     ZAI-F12             TO   BO012 (IX01).
     MOVE     ZAI-F13             TO   BO013 (IX01).
     IF       MM                  =    12
         MOVE  ZAI-F152 (MM)      TO   BO009 (IX01)
         MOVE  ZAI-F153 (MM)      TO   BO011 (IX01)
     ELSE
         MOVE  ZAI-F162 (MM)      TO   BO009 (IX01)
         MOVE  ZAI-F163 (MM)      TO   BO011 (IX01)
     END-IF.
 BODY-SET-EXIT.
     EXIT.
****************************************************************
*             ＰＦキーガイダンス
****************************************************************
 PFK-DSP-SEC            SECTION.
     IF       SYORI-GROUP         =   "HEAD"
         MOVE  PMSG01             TO   MO002
     ELSE
         MOVE  SPACE              TO   PMSG02R
         MOVE  PMSG02             TO   PMSG02A
         MOVE  1                  TO   IX01
         IF   PF07-KEY       NOT  =    LOW-VALUE
           MOVE  NC"_"           TO   PMSG02IX ( IX01     )
           MOVE  NC"前"           TO   PMSG02IX ( IX01 + 1 )
           MOVE  NC"頁"           TO   PMSG02IX ( IX01 + 2 )
           ADD   4                TO   IX01
         END-IF
         IF   PF08-KEY       NOT  =    HIGH-VALUE
           MOVE  NC"_"           TO   PMSG02IX ( IX01     )
           MOVE  NC"次"           TO   PMSG02IX ( IX01 + 1 )
           MOVE  NC"頁"           TO   PMSG02IX ( IX01 + 2 )
           ADD   4                TO   IX01
         END-IF
         IF   PF11-KEY       NOT  =    LOW-VALUE
           MOVE  NC"_"           TO   PMSG02IX ( IX01     )
           MOVE  NC"前"           TO   PMSG02IX ( IX01 + 1 )
           MOVE  NC"レ"           TO   PMSG02IX ( IX01 + 2 )
           MOVE  NC"コ"           TO   PMSG02IX ( IX01 + 3 )
           MOVE  NC"ー"           TO   PMSG02IX ( IX01 + 4 )
           MOVE  NC"ド"           TO   PMSG02IX ( IX01 + 5 )
           ADD   7                     TO   IX01
         END-IF
         IF   PF12-KEY       NOT  =    HIGH-VALUE
           MOVE  NC"_"           TO   PMSG02IX ( IX01     )
           MOVE  NC"次"           TO   PMSG02IX ( IX01 + 1 )
           MOVE  NC"レ"           TO   PMSG02IX ( IX01 + 2 )
           MOVE  NC"コ"           TO   PMSG02IX ( IX01 + 3 )
           MOVE  NC"ー"           TO   PMSG02IX ( IX01 + 4 )
           MOVE  NC"ド"           TO   PMSG02IX ( IX01 + 5 )
         END-IF
         MOVE  PMSG02R            TO   MO002
     END-IF.
     MOVE    "MO002"                   TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
 PFK-DSP-EXIT.
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
     MOVE    "MO001"                   TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
 ERR-DSP-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE         ZZAIMS2
                   ZZAIMS3
                   HMEIMS
                   HJYOKEN
                   ZSOKMS
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
     MOVE     SPACE               TO   DSP-PRO.
     WRITE    ZSY0010.
 DSP-WR-EXIT.
     EXIT.

```
