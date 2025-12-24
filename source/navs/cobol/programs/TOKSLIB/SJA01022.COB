****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＪＡＮ管理システム　　　　　　　　*
*    モジュール名　　　　：　商品コード照会　　　　　　　　　　*
*    作成日／作成者　　　：　00/01/05  /HAGIWARA               *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SJA0102I.
 AUTHOR.                   HAGIWARA.
 DATE-WRITTEN.             00/01/05.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          FUJITU.
 OBJECT-COMPUTER.          FUJITU.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*ＪＡＮ管理ファイル１（KEY:F01 JANｺｰﾄﾞ)
     SELECT  JANKANF1  ASSIGN    TO        JANKANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       JAN1-F01
                       POSITIONING POINTER JAN1-POS
                       FILE      STATUS    JAN1-ST.
*ＪＡＮ管理ファイル２（KEY:F04 ﾋﾝﾒｲｶﾅ)
     SELECT  JANKANF2  ASSIGN    TO        JANKANL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       JAN2-F04
                       POSITIONING POINTER JAN2-POS
                       FILE      STATUS    JAN2-ST.
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
****************************************************************
*    FILE = ＪＡＮ管理ファイル１                               *
****************************************************************
 FD  JANKANF1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JANKANL1  OF   XFDLIB
                       JOINING   JAN1      AS   PREFIX.
****************************************************************
*    FILE = ＪＡＮ管理ファイル２                               *
****************************************************************
 FD  JANKANF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JANAKNL2  OF   XFDLIB
                       JOINING   JAN2      AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJA01021  OF   XMDLIB
***                    JOINING   DSP       AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
     COPY        JANKANF   OF        XFDLIB
     JOINING     JAN       AS        PREFIX.
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
*
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  JAN1-ST           PIC X(02).
     03  JAN2-ST           PIC X(02).
     03  JAN1-POS          PIC 9(09) COMP VALUE 0.
     03  JAN2-POS          PIC 9(09) COMP VALUE 0.
*
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
     03  WK-JAN-KEY        PIC X(30).
     03  WK-JAN-POS        PIC 9(09) COMP.
     03  WK-HEAD-KEY       PIC X(30).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  IX01              PIC 9(02) VALUE     ZERO.
     03  IX02              PIC 9(02) VALUE     ZERO.
     03  ERR-NO            PIC 9(02) VALUE     ZERO.

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
 01  WK-JIKAN.
     03  WK-HH                    PIC  9(02)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
*
*    03  FILE-ERR1         PIC N(20) VALUE
*                    NC"画面ファイル異常！".
*    03  FILE-ERR2         PIC N(20) VALUE
*                    NC"商品名称マスタ（商品コード）異常！".
*    03  FILE-ERR3         PIC N(20) VALUE
*                    NC"商品名称マスタ（商品カナ名）異常！".
*    03  FILE-ERR4         PIC N(20) VALUE
*                    NC"商品名称マスタ（ＪＡＮコード）異常！".
*    03  FILE-ERR5         PIC N(20) VALUE
*                    NC"仕入先マスタ異常！".
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JAN1-ERR           PIC N(15) VALUE
                        NC"ＪＡＮ管理Ｆ１エラー".
     03  JAN2-ERR           PIC N(15) VALUE
                        NC"ＪＡＮ管理Ｆ２エラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
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
 JAN1-ERR                    SECTION.
     USE         AFTER      EXCEPTION PROCEDURE JANKANF1.
     MOVE        JAN1-ST     TO        E-ST.
     MOVE        "JANKANF1" TO        E-FILE.
     DISPLAY     SEC-NAME   UPON      CONS.
     DISPLAY     ERR-FILE   UPON      CONS.
     DISPLAY     ERR-NAME   UPON      CONS.
     DISPLAY     JAN1-ERR    UPON      CONS.
     MOVE        "4000"     TO        PROGRAM-STATUS.
     STOP        RUN.
 JAN2-ERR                    SECTION.
     USE         AFTER      EXCEPTION PROCEDURE JANKANF2.
     MOVE        JAN2-ST     TO        E-ST.
     MOVE        "JANKANF2" TO        E-FILE.
     DISPLAY     SEC-NAME   UPON      CONS.
     DISPLAY     ERR-FILE   UPON      CONS.
     DISPLAY     ERR-NAME   UPON      CONS.
     DISPLAY     JAN2-ERR    UPON      CONS.
     MOVE        "4000"     TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
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
     MOVE     "INIT-SEC"     TO   S-NAME.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN      INPUT            JANKANF1  JANKANF2.
     OPEN      I-O              DSPFILE.
*
     PERFORM            DSP-INIT-SEC.
     MOVE    "HEAD"               TO   SYORI-GROUP.
 INIT-EXIT.
     EXIT.
******************************************************************
*             初期画面　表示
******************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    "DSP-INIT-SEC"       TO   S-NAME.
*
     MOVE     SPACE               TO   FJA01021.
     MOVE     SPACE               TO   DSP-AREA.
     MOVE    "FJA01021"           TO   DSP-FMT.
     MOVE    "SCREEN"             TO   DSP-GRP.
     MOVE    "CL"                 TO   DSP-PRO.
     WRITE    FJA01021.
 DSP-INIT-EXIT.
     EXIT.
******************************************************************
*             主処理
******************************************************************
 MAIN-SEC               SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
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
     MOVE    "HEAD-SEC"           TO   S-NAME.
*ヘッド部入力
     MOVE     PMSG01              TO   ERRMSG.
     MOVE    "SCREEN"             TO   DSP-GRP.
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
     MOVE    "M"                  TO   EDIT-OPTION OF HJANCD.
     MOVE    "M"                  TO   EDIT-OPTION OF HKANA.
     MOVE    " "                  TO   EDIT-CURSOR OF HJANCD.
     MOVE    " "                  TO   EDIT-CURSOR OF HKANA.
     IF       HJANCD              =    SPACE
     AND      HKANA               =    SPACE
              MOVE   3            TO   ERR-NO
              MOVE   "C"          TO   EDIT-CURSOR  OF  HJANCD
     END-IF.
     MOVE     HIGH-VALUE          TO   START-KEY.
     MOVE     0                   TO   START-POS    END-POS.
*
     EVALUATE TRUE
     WHEN     HJANCD         NOT  =    SPACE
         MOVE  "1"                TO   SYORI-FLG
         MOVE  HJANCD             TO   END-KEY  WK-HEAD-KEY
         IF   HKANA               NOT  =    SPACE
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-CURSOR  OF  HJANCD
              MOVE   "R"          TO   EDIT-CURSOR  OF  HKANA
              MOVE   "C"          TO   EDIT-CURSOR  OF  HJANCD
         END-IF
     WHEN     HKANA          NOT  =    SPACE
         MOVE  "2"                TO   SYORI-FLG
         MOVE  HKANA              TO   END-KEY  WK-HEAD-KEY
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
     MOVE     "BODY-SEC"          TO   S-NAME.
*ボディ部入力
     IF       START-KEY           =    LOW-VALUE
         IF   END-KEY             =    HIGH-VALUE
              MOVE  PMSG02        TO   ERRMSG
         ELSE
              MOVE  PMSG04        TO   ERRMSG
         END-IF
     ELSE
         IF   END-KEY             =    HIGH-VALUE
              MOVE  PMSG03        TO   ERRMSG
         ELSE
              MOVE  PMSG05        TO   ERRMSG
         END-IF
     END-IF.
     MOVE    "SCREEN"             TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     MOVE    "BODY"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   BODY-EXIT
     WHEN         "F004"
*NA********** PERFORM   DSP-INIT-SEC
              MOVE     SPACE      TO   FJA01021
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   BODY-EXIT
**** WHEN         "F006"
****          MOVE     "HEAD"     TO   SYORI-GROUP
****                              GO   TO   BODY-EXIT
     WHEN         "E000"
              CONTINUE
     WHEN         "F011"
              PERFORM   BEFORE-PAGE-SEC
     WHEN         "F012"
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
     MOVE     "BEFORE-PAGE-SEC"   TO   S-NAME.
*
     IF       START-KEY           =    LOW-VALUE
              MOVE  5             TO   ERR-NO
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     ZERO                TO   INV-SW.
     MOVE     START-KEY           TO   WK-JAN-KEY.
     MOVE     START-POS           TO   WK-JAN-POS.
     PERFORM            JANMS-START-RVS.
     IF       INV-SW              =    0
     AND      START-KEY           =    WK-JAN-KEY
     AND      START-POS           =    WK-JAN-POS
         PERFORM        JANMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
         MOVE  5                  TO   ERR-NO
         MOVE  LOW-VALUE          TO   START-KEY
                                  GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     PERFORM       VARYING   IX01      FROM      8    BY   -1
                   UNTIL     IX01           <    1
                   OR        INV-SW         =    1
                   OR        WK-HEAD-KEY    >    WK-JAN-KEY
         MOVE  WK-JAN-KEY         TO   START-KEY
         MOVE  WK-JAN-POS         TO   START-POS
         PERFORM        JANMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
     OR       WK-HEAD-KEY         >    WK-JAN-KEY
         MOVE  WK-HEAD-KEY        TO   WK-JAN-KEY
         MOVE  0                  TO   WK-JAN-POS
         MOVE  LOW-VALUE          TO   START-KEY
         MOVE  0                  TO   START-POS
         MOVE  ZERO               TO   INV-SW
         PERFORM        JANMS-INIT-START
     ELSE
         MOVE  START-KEY          TO   WK-JAN-KEY
         MOVE  START-POS          TO   WK-JAN-POS
         PERFORM        JANMS-START-SEC
     END-IF.
     MOVE     SPACE               TO   BODYG.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    8
                   OR        INV-SW         =    1
         PERFORM        BODY-SET-SEC
         MOVE  WK-JAN-KEY         TO   END-KEY
         MOVE  WK-JAN-POS         TO   END-POS
         PERFORM        JANMS-READ-SEC
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
     MOVE     END-KEY             TO   WK-JAN-KEY.
     MOVE     END-POS             TO   WK-JAN-POS.
     IF       SYORI-GROUP         =   "HEAD"
         PERFORM        JANMS-INIT-START
     ELSE
         PERFORM        JANMS-START-SEC
     END-IF.
     IF       INV-SW              =    0
     AND      END-KEY             =    WK-JAN-KEY
     AND      END-POS             =    WK-JAN-POS
     AND      SYORI-GROUP         =   "BODY"
         PERFORM        JANMS-READ-SEC
     END-IF.
     IF       INV-SW              =    1
              MOVE  6             TO   ERR-NO
                                  GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     WK-JAN-KEY          TO   START-KEY.
     MOVE     WK-JAN-POS          TO   START-POS.
     MOVE     SPACE               TO   BODYG.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    8
                   OR        INV-SW         =    1
         PERFORM        BODY-SET-SEC
         MOVE  WK-JAN-KEY         TO   END-KEY
         MOVE  WK-JAN-POS         TO   END-POS
         PERFORM        JANMS-READ-SEC
     END-PERFORM.
     IF       INV-SW              =    1
         MOVE  HIGH-VALUE         TO   END-KEY
     END-IF.
 NEXT-PAGE-EXIT.
     EXIT.
******************************************************************
*             ＪＡＮ管理マスタ位置付け（降順）
******************************************************************
 JANMS-START-RVS        SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-JAN-KEY             TO   JAN1-F01
         MOVE  WK-JAN-POS             TO   JAN1-POS
         START    JANKANF1
             POSITIONING    POINTER   IS   JAN1-POS
                                 KEY  =    JAN1-F01
                                 WITH      REVERSED  ORDER
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-JAN-KEY             TO   JAN2-F04
         MOVE  WK-JAN-POS             TO   JAN2-POS
         START    JANKANF2
             POSITIONING    POINTER   IS   JAN2-POS
                                 KEY  =    JAN2-F04
                                 WITH      REVERSED  ORDER
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
     IF       INV-SW                  =    0
         PERFORM       JANMS-READ-SEC
     END-IF.
 JANMS-START-EXIT.
     EXIT.
******************************************************************
*             ＪＡＮ管理マスタ位置付け（昇順）
******************************************************************
 JANMS-START-SEC        SECTION.
     MOVE         "JANMS-START-SEC"   TO   S-NAME.
*
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-JAN-KEY             TO   JAN1-F01
         MOVE  WK-JAN-POS             TO   JAN1-POS
         START    JANKANF1
             POSITIONING    POINTER   IS   JAN1-POS
                                 KEY  =    JAN1-F01
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-JAN-KEY             TO   JAN2-F04
         MOVE  WK-JAN-POS             TO   JAN2-POS
         START    JANKANF2
             POSITIONING    POINTER   IS   JAN2-POS
                                 KEY  =    JAN2-F04
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
*
     IF       INV-SW                  =    0
         PERFORM       JANMS-READ-SEC
     END-IF.
 JANMS-START-EXIT.
     EXIT.
******************************************************************
*             ＪＡＮ管理マスタ位置付け
******************************************************************
 JANMS-INIT-START       SECTION.
     MOVE         "JANMS-INIT-START"        TO   S-NAME.
*
     EVALUATE SYORI-FLG
     WHEN     1
         MOVE  WK-JAN-KEY             TO   JAN1-F01
         MOVE  0                      TO   JAN1-POS
         START    JANKANF1       KEY  >=   JAN1-F01
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     WHEN     2
         MOVE  WK-JAN-KEY             TO   JAN2-F04
         MOVE  0                      TO   JAN2-POS
         START    JANKANF2       KEY  >=   JAN2-F04
             INVALID  KEY
                  MOVE  1             TO   INV-SW
         END-START
     END-EVALUATE.
*
     IF       INV-SW                  =    0
         PERFORM       JANMS-READ-SEC
     END-IF.
 JANMS-START-EXIT.
     EXIT.
******************************************************************
*             ＪＡＮ管理マスタ読込み
******************************************************************
 JANMS-READ-SEC         SECTION.
     EVALUATE SYORI-FLG
     WHEN     1
         READ     JANKANF1        NEXT
             AT   END
                  MOVE  1             TO   INV-SW
             NOT  AT   END
                  MOVE  JAN1-F01      TO   WK-JAN-KEY
                  MOVE  0             TO   WK-JAN-POS
                  MOVE  JAN1-REC      TO   JAN-REC
         END-READ
     WHEN     2
         READ     JANKANF2        NEXT
             AT   END
                  MOVE  1             TO   INV-SW
             NOT  AT   END
                  MOVE  JAN2-F04      TO   WK-JAN-KEY
                  MOVE  JAN2-POS      TO   WK-JAN-POS
                  MOVE  JAN2-REC      TO   JAN-REC
         END-READ
     END-EVALUATE.
 JANMS-READ-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.
     MOVE     "BODY-SET-SEC"     TO   S-NAME.
*
     MOVE     JAN-F01            TO   MJANCD(IX01).
     MOVE     JAN-F03(1:10)      TO   MHINNM1(IX01).
     MOVE     JAN-F03(11:10)     TO   MHINNM2(IX01).
     MOVE     JAN-F04            TO   MKANA(IX01).
**** READ     ZSHIMS
****     INVALID
****          MOVE  ALL NC"＊"   TO   BO006 (IX01)
****     NOT  INVALID
****          MOVE  SHI-F02      TO   BO006 (IX01)
**** END-READ.
 BODY-SET-EXIT.
     EXIT.
****************************************************************
*             エラー処理
****************************************************************
 ERR-DSP-SEC            SECTION.
     IF       ERR-NO              =    ZERO
              MOVE  SPACE              TO   ERRMSG
     ELSE
              MOVE  MSGIX (ERR-NO)     TO   ERRMSG
     END-IF.
*NA* MOVE    "ERRMSG"                   TO   DSP-GRP.
*NA* PERFORM            DSP-WR-SEC.
 ERR-DSP-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE         JANKANF1
                   JANKANF2
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
     WRITE    FJA01021.
 DSP-WR-EXIT.
     EXIT.
