# STA0030I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/STA0030I.COB`

## ソースコード

```cobol
***************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　商品_卸入力　　　　　　　　　　　*
*    作成日／更新日　　　：　00/05/29                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*                                                              *
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               STA0030I.
 AUTHOR.                   N.T.
 DATE-WRITTEN.             00/05/29.
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
*_卸ファイル１
     SELECT      ZTANADT   ASSIGN    TO        DA-01-VI-ZTANADT1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TANA-F01
                           FILE      STATUS    TANA-ST   TANA-ST1.
*_卸ファイル２
     SELECT      ZTANADT2  ASSIGN    TO        DA-01-VI-ZTANADT2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F04
                                               TAN-F05
                                               TAN-F06X
                                               TAN-F15
                           FILE      STATUS    TAN-ST   TAN-ST1.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYOKEN-F01
                                               JYOKEN-F02
                           FILE      STATUS    JYKN-ST   JYKN-ST1.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F01
                           FILE      STATUS    MEI-ST    MEI-ST1.
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
*_卸ファイル１
 FD  ZTANADT
     LABEL       RECORD    IS        STANDARD.
     COPY        ZTANADT   OF        XFDLIB
     JOINING     TANA      AS        PREFIX.
*_卸ファイル２
 FD  ZTANADT2
     LABEL       RECORD    IS        STANDARD.
     COPY        ZTANADT   OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYOKEN    AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FTA00301  OF        XMDLIB.
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
* エラーメッセージエリア
 01  MSG-AREA.
     03  MSG01             PIC N(15) VALUE
                     NC"原票_が登録されていません".
     03  MSG02             PIC N(15) VALUE
                     NC"明細行がすべて削除されています".
     03  MSG03             PIC N(15) VALUE
                     NC"処理区分が違います".
     03  MSG04             PIC N(15) VALUE
                     NC"原票_が未入力です".
     03  MSG05             PIC N(15) VALUE SPACE.
     03  MSG06             PIC N(15) VALUE
                     NC"他の倉庫データです".
     03  MSG07             PIC N(15) VALUE
                     NC"Ｙ又はＨで入力してください".
     03  MSG08             PIC N(15) VALUE
                     NC"次の原票_は　削除されています".
     03  MSG09             PIC N(15) VALUE
                     NC"次の原票_は　存在しません".
     03  MSG10             PIC N(15) VALUE
                     NC"無効ＰＦキーです".
     03  MSG11             PIC N(15) VALUE
                     NC"倉庫マスタが存在しません".
     03  MSG12             PIC N(15) VALUE
                     NC"指定倉庫コードは扱えません".
     03  MSG13             PIC N(15) VALUE
                     NC"商品名称マスタが存在しません".
     03  MSG14             PIC N(15) VALUE
                     NC"単価が未入力です".
     03  MSG15             PIC N(15) VALUE
                     NC"削除区分は９を入力してください".
     03  MSG16             PIC N(15) VALUE
                     NC"明細行すべて登録済です".
     03  MSG17             PIC N(15) VALUE
                     NC"倉庫，商品，品単，_番で登録済".
     03  MSG18             PIC N(15) VALUE
                     NC"商品，品単、_番が重複してます".
     03  MSG19             PIC N(15) VALUE
                     NC"入力エラーです".
     03  MSG20             PIC N(15) VALUE
                     NC"明細が入力されていません".
 01  MSG-AREAR             REDEFINES MSG-AREA.
     03  MSGIX             PIC N(15) OCCURS    20.
* ＰＦキーガイド
 01  PFKEY-MSG.
     03  PMSG01            PIC N(15) VALUE
                     NC"_取消　_終了".
     03  PMSG02            PIC N(15) VALUE
                     NC"_取消　_終了　_項目戻し".
* ＷＫ・フラグエリア
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  SYORI-GROUP       PIC X(04).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  IX01              PIC 9(02) VALUE     ZERO.
     03  IX02              PIC 9(02) VALUE     ZERO.
     03  IX03              PIC 9(02) VALUE     ZERO.
     03  ERR-NO            PIC 9(02) VALUE     ZERO.
     03  NOT-DELETE        PIC 9(02) VALUE     ZERO.
**** 端末識別
     03  WK-JYO-SOKCD      PIC X(02) VALUE     SPACE.
     03  WK-DAISOKO        PIC X(02) VALUE     SPACE.
**** 品単ＳＰＡＣＥ詰めＷＫ
     03  WK-BI005          PIC X(05) VALUE     SPACE.
****
     03  BODY-CHECK        PIC X(02) VALUE     SPACE.
**** 商品名称ＷＫ
     03  SYOHIN-NAMEX.
       05  SYOHIN-NAME     PIC N(30).
****
     03  LINE-CNT          PIC 9(02) VALUE     ZERO.
**** ＢＯＤＹチェックＦＬＧ　明細有無・削除ＦＬＧ・入力有無
     03  TBL.
       05  WK-TBL          OCCURS     10.
         07  TB            PIC 9(01).
         07  WK-F99        PIC X(01).
**** 新規登録フラグ
     03  WK-TOROKU         PIC X(01)   VALUE  SPACE.
**** ＳＴＡＴＵＳ　エリア　
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TANA-ST           PIC X(02).
     03  TANA-ST1          PIC X(04).
     03  TAN-ST            PIC X(02).
     03  TAN-ST1           PIC X(04).
     03  JYKN-ST           PIC X(02).
     03  JYKN-ST1          PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
**** ファイルエラーメッセージ
     03  FILE-ERR1         PIC N(10) VALUE
                     NC"_卸ファイル１異常！".
     03  FILE-ERR6         PIC N(10) VALUE
                     NC"_卸ファイル２異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                     NC"条件ファイル異常！".
     03  FILE-ERR4         PIC N(10) VALUE
                     NC"商品名称マスタ異常！".
     03  FILE-ERR5         PIC N(10) VALUE
                     NC"倉庫マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                     NC"画面ファイル異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 LINKAGE                SECTION.
******************************************************************
 01  LINK-AREA.
     03  LINK-WS-NAME      PIC X(08).
******************************************************************
 PROCEDURE              DIVISION       USING     LINK-AREA.
******************************************************************
*             ファイルエラー処理
******************************************************************
 DECLARATIVES.
 TANA-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZTANADT.
     DISPLAY       TANA-ST   UPON      STA.
     DISPLAY       FILE-ERR1 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 TAN-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZTANADT2.
     DISPLAY       TAN-ST    UPON      STA.
     DISPLAY       FILE-ERR1 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 JYKN-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HJYOKEN.
     DISPLAY       JYKN-ST   UPON      STA.
     DISPLAY       FILE-ERR3 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         HMEIMS.
     DISPLAY       MEI-ST    UPON      STA.
     DISPLAY       FILE-ERR4 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 SOK-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZSOKMS.
     DISPLAY       SOK-ST    UPON      STA.
     DISPLAY       FILE-ERR5 UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 DSP-ERR                SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
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
     OPEN     INPUT     HJYOKEN
                        ZSOKMS
                        HMEIMS
                        ZTANADT2
              I-O       ZTANADT
                        DSPFILE.
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
* 条件ファイル（端末識別）
     MOVE     65                  TO   JYOKEN-F01.
     MOVE     LINK-WS-NAME        TO   JYOKEN-F02.
     READ     HJYOKEN
         INVALID
              DISPLAY "* HJYOKEN  INVALID KEY:"
                   JYOKEN-F01 "," JYOKEN-F02 "*" UPON STA
              MOVE  9             TO   END-FLG
                                  GO   TO   INIT-EXIT
         NOT  INVALID
              MOVE  JYOKEN-F14(1:2)   TO   WK-JYO-SOKCD
              MOVE  JYOKEN-F15(1:2)   TO   WK-DAISOKO
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
     MOVE     SPACE               TO   FTA00301.
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
*
     MOVE     SPACE               TO   DSP-AREA.
     MOVE    "FTA00301"           TO   DSP-FMT.
     MOVE    "SCREEN"             TO   DSP-GRP.
     MOVE    "CL"                 TO   DSP-PRO.
     WRITE    FTA00301.
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
       WHEN       "TAIL"
              PERFORM        TAIL-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
******************************************************************
*             ヘッド処理
******************************************************************
 HEAD-SEC               SECTION.
*ヘッド部入力
     MOVE     ZERO                TO   TBL.
     MOVE     PMSG01              TO   MO002.
     PERFORM            DSP-WR-SEC.
     MOVE    "HEAD"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
* アテンション判定
     EVALUATE      DSP-FNC
         WHEN      "F004"
              MOVE     SPACE      TO   FTA00301
              MOVE   "M"          TO   EDIT-OPTION  OF  HI001
              MOVE   " "          TO   EDIT-CURSOR  OF  HI001
              MOVE   "M"          TO   EDIT-OPTION  OF  HI002
              MOVE   " "          TO   EDIT-CURSOR  OF  HI002
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   HEAD-EXIT
         WHEN      "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   HEAD-EXIT
         WHEN      "E000"
              CONTINUE
         WHEN       OTHER
              MOVE   10           TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   HEAD-EXIT
     END-EVALUATE.
*処理区分 CHECK
     IF     ( HI001                    NUMERIC  )
     AND    ( HI001               =    1  OR  2 )
              MOVE   "M"          TO   EDIT-OPTION  OF  HI001
              MOVE   " "          TO   EDIT-CURSOR  OF  HI001
              IF   HI001          =    1
                   MOVE  NC"登録"      TO   HO001
              ELSE
                   MOVE  NC"修正"      TO   HO001
              END-IF
     ELSE
              MOVE   3            TO   ERR-NO
              MOVE   "R"          TO   EDIT-OPTION  OF  HI001
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI001
     END-IF.
*原票ＮＯ CHECK
     IF       HI002          NOT       NUMERIC
              IF   ERR-NO         =    ZERO
                   MOVE  4        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-OPTION  OF  HI002
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
              PERFORM   ERR-DSP-SEC
                                  GO   TO   HEAD-EXIT
     ELSE
              MOVE   "M"          TO   EDIT-OPTION  OF  HI002
              MOVE   " "          TO   EDIT-CURSOR  OF  HI002
     END-IF.
*　　_卸ファイルをチェックし，ボディー部をセット
     MOVE     ZERO                TO   INV-SW    NOT-DELETE.
     MOVE     HI002               TO   TANA-F01.
     COMPUTE  TANA-F01            =    TANA-F01  *  10.
     PERFORM            TANA-START-SEC.
     MOVE    "M"                  TO   EDIT-OPTION  OF  HI002.
     MOVE    " "                  TO   EDIT-CURSOR  OF  HI002.
* _卸Ｆ　ＳＴＡＲＴ　ＯＫで原票_（_卸Ｆ）＝入力値
     IF     ( INV-SW         =    0 )       AND
            ( HI002          =    TANA-F01(1:6) )
*        本社以外の端末の時、他倉庫データはエラー
         IF   ( WK-DAISOKO     NOT  =   "01" )    AND
              ( TANA-F04       NOT  =    WK-JYO-SOKCD )
              IF   ERR-NO         =    ZERO
                   MOVE  6        TO   ERR-NO
              END-IF
              MOVE   "R"          TO   EDIT-OPTION  OF  HI002
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
         ELSE
              PERFORM        BODY-SET-SEC
              IF   NOT-DELETE     =    0
                   IF   ERR-NO    =    ZERO
                        MOVE  2   TO   ERR-NO
                   END-IF
                   MOVE   "R"     TO   EDIT-OPTION  OF  HI002
                   MOVE   "C"     TO   EDIT-CURSOR  OF  HI002
              END-IF
         END-IF
     ELSE
         IF   HI001               =    2
              IF   ERR-NO         =    ZERO
                   MOVE  1        TO   ERR-NO
              END-IF
              PERFORM   ERR-DSP-SEC
              MOVE   "R"          TO   EDIT-OPTION  OF  HI002
              MOVE   "C"          TO   EDIT-CURSOR  OF  HI002
         END-IF
         IF   HI001               =    1      AND
              WK-DAISOKO      NOT =   "01"
              MOVE    WK-JYO-SOKCD      TO  HI003  SOK-F01
              READ   ZSOKMS
                  INVALID      MOVE  SPACE     TO  HO002
                  NOT INVALID  MOVE  SOK-F02   TO  HO002
              END-READ
         END-IF
     END-IF.
******
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    0
         IF   NOT-DELETE          >    0
              PERFORM   DSP-WR-SEC
         END-IF
         MOVE  "BODY"              TO   SYORI-GROUP
     END-IF.
 HEAD-EXIT.
     EXIT.
******************************************************************
*             _卸ファイル位置付け
******************************************************************
 TANA-START-SEC         SECTION.
     START    ZTANADT        KEY  >=   TANA-F01
         INVALID  KEY
              MOVE  1             TO   INV-SW
         NOT  INVALID
              PERFORM   TANA-READ-SEC
     END-START.
 TANA-START-EXIT.
     EXIT.
******************************************************************
*             _卸ファイル読込み
******************************************************************
 TANA-READ-SEC          SECTION.
     READ     ZTANADT   NEXT
         AT   END
              MOVE  1             TO   INV-SW
     END-READ.
 TANA-READ-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.
* ＢＯＤＹクリア
     MOVE     SPACE               TO   BODYG     BODY-CHECK.
* 倉庫マスタＲＥＡＤ
     MOVE     TANA-F04            TO   HI003     SOK-F01.
     READ     ZSOKMS
         INVALID
              MOVE  ALL NC"＊"    TO   HO002
         NOT  INVALID
              MOVE  SOK-F02       TO   HO002
     END-READ.
*プロテクトセット
     MOVE   "X"                   TO   EDIT-STATUS OF HI003.
     MOVE   "X"                   TO   EDIT-STATUS OF HO002.
*明細セット
     MOVE   ZERO                  TO   TBL.
     PERFORM  UNTIL   ( INV-SW         =    1 )
              OR      ( HI002     NOT  =    TANA-F01(1:6) )
         ADD   1                  TO   NOT-DELETE
         MOVE  TANA-F01 (7:1)     TO   IX01
         ADD   1                  TO   IX01
         MOVE  1                  TO   TB(IX01)
         MOVE  TANA-F99           TO   WK-F99(IX01)
         ADD   1                  TO   LINE-CNT
         MOVE  TANA-F15 (1:1)     TO   BI001 (IX01)
         MOVE  TANA-F15 (2:3)     TO   BI002 (IX01)
         MOVE  TANA-F15 (5:2)     TO   BI003 (IX01)
         MOVE  TANA-F05           TO   BI004 (IX01)
         MOVE  TANA-F06           TO   BI005 (IX01)
         MOVE  TANA-F07           TO   BI006 (IX01)
         MOVE  TANA-F08           TO   BI007 (IX01)
         MOVE  TANA-F09           TO   BI008 (IX01)
         MOVE  TANA-F09 (5:1)     TO   BI009 (IX01)
         MOVE  TANA-F16           TO   BO001 (IX01)
         MOVE  TANA-F11           TO   BI010 (IX01)
         MOVE  TANA-F12           TO   BI011 (IX01)
         MOVE  TANA-F13           TO   BI012 (IX01)
         MOVE  TANA-F99           TO   BI013 (IX01)
         MOVE  TANA-F14           TO   BI014 (IX01)
         MOVE  TANA-F05           TO   MEI-F011
         MOVE  TANA-F06           TO   MEI-F012 (1:5)
         MOVE  TANA-F07           TO   MEI-F012 (6:2)
         MOVE  TANA-F08           TO   MEI-F012 (8:1)
         READ      HMEIMS
             INVALID
                MOVE  ALL NC"＊"  TO   BO002 (IX01)
             NOT  INVALID
                MOVE  MEI-F02     TO   SYOHIN-NAMEX
                MOVE  SYOHIN-NAME TO   BO002 (IX01)
         END-READ
**   登録時はデータ有りの時プロテクト
         IF    HI001   =   1
               MOVE  "X"          TO   EDIT-STATUS OF BI001 (IX01)
                                       EDIT-STATUS OF BI002 (IX01)
                                       EDIT-STATUS OF BI003 (IX01)
                                       EDIT-STATUS OF BI004 (IX01)
                                       EDIT-STATUS OF BI005 (IX01)
                                       EDIT-STATUS OF BI006 (IX01)
                                       EDIT-STATUS OF BI007 (IX01)
                                       EDIT-STATUS OF BI008 (IX01)
                                       EDIT-STATUS OF BI009 (IX01)
                                       EDIT-STATUS OF BO001 (IX01)
                                       EDIT-STATUS OF BI010 (IX01)
                                       EDIT-STATUS OF BI011 (IX01)
                                       EDIT-STATUS OF BI012 (IX01)
                                       EDIT-STATUS OF BI013 (IX01)
                                       EDIT-STATUS OF BI014 (IX01)
                                       EDIT-STATUS OF BO002 (IX01)
         END-IF
*      _卸ファイル読込み
         PERFORM   TANA-READ-SEC
     END-PERFORM.
* 登録時１０行登録済の時エラー
     IF   ( HI001      =   1  )  AND
          ( LINE-CNT   =   10 )
          IF    ERR-NO   =   ZERO
             MOVE  16                  TO   ERR-NO
          END-IF
          MOVE   "C"               TO  EDIT-CURSOR OF HI002
          MOVE   "X"               TO  EDIT-STATUS OF BI001 (IX01)
                                       EDIT-STATUS OF BI002 (IX01)
                                       EDIT-STATUS OF BI003 (IX01)
                                       EDIT-STATUS OF BI004 (IX01)
                                       EDIT-STATUS OF BI005 (IX01)
                                       EDIT-STATUS OF BI006 (IX01)
                                       EDIT-STATUS OF BI007 (IX01)
                                       EDIT-STATUS OF BI008 (IX01)
                                       EDIT-STATUS OF BI009 (IX01)
                                       EDIT-STATUS OF BO001 (IX01)
                                       EDIT-STATUS OF BI010 (IX01)
                                       EDIT-STATUS OF BI011 (IX01)
                                       EDIT-STATUS OF BI012 (IX01)
                                       EDIT-STATUS OF BI013 (IX01)
                                       EDIT-STATUS OF BI014 (IX01)
                                       EDIT-STATUS OF BO002 (IX01)
     END-IF.
**
     MOVE   ZERO                       TO   LINE-CNT.
**
 BODY-SET-EXIT.
     EXIT.
******************************************************************
*             ボディ処理
******************************************************************
 BODY-SEC               SECTION.
*ボディ部入力
     MOVE     PMSG02              TO   MO002.
     PERFORM            DSP-WR-SEC.
* 登録時削除フラグプロテクト、修正時は
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    10
         IF   HI001               =    1
              MOVE   "X"          TO   EDIT-STATUS OF BI013 (IX01)
         ELSE
              IF   TB(IX01)     =     0
                   MOVE   "X"     TO   EDIT-STATUS OF BI008 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI009 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI010 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI011 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI012 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI013 (IX01)
                   MOVE   "X"     TO   EDIT-STATUS OF BI014 (IX01)
              END-IF
         END-IF
     END-PERFORM.
*
     IF       HI001               =    1
              MOVE  "BODY"        TO   DSP-GRP
     ELSE
              MOVE  "UPMODE"      TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
* アテンション判定
     EVALUATE      DSP-FNC
         WHEN      "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   BODY-EXIT
         WHEN         "F004"
              MOVE     SPACE      TO   FTA00301
              PERFORM   BODY-ATTR-CLEAR
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   BODY-EXIT
         WHEN      "F006"
              PERFORM   BODY-ATTR-CLEAR
              PERFORM   ERR-DSP-SEC
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   BODY-EXIT
         WHEN      "E000"
              PERFORM    VARYING   IX01   FROM   1   BY   1
                         UNTIL     IX01     >    10
                 MOVE    "M"      TO   EDIT-OPTION OF BI001(IX01)
                                       EDIT-OPTION OF BI002(IX01)
                                       EDIT-OPTION OF BI003(IX01)
                                       EDIT-OPTION OF BI004(IX01)
                                       EDIT-OPTION OF BI005(IX01)
                                       EDIT-OPTION OF BI006(IX01)
                                       EDIT-OPTION OF BI007(IX01)
                                       EDIT-OPTION OF BI008(IX01)
                                       EDIT-OPTION OF BI009(IX01)
                                       EDIT-OPTION OF BI010(IX01)
                                       EDIT-OPTION OF BI011(IX01)
                                       EDIT-OPTION OF BI012(IX01)
                                       EDIT-OPTION OF BI013(IX01)
                                       EDIT-OPTION OF BI014(IX01)
                 MOVE    " "      TO   EDIT-CURSOR OF BI001(IX01)
                                       EDIT-CURSOR OF BI002(IX01)
                                       EDIT-CURSOR OF BI003(IX01)
                                       EDIT-CURSOR OF BI004(IX01)
                                       EDIT-CURSOR OF BI005(IX01)
                                       EDIT-CURSOR OF BI006(IX01)
                                       EDIT-CURSOR OF BI007(IX01)
                                       EDIT-CURSOR OF BI008(IX01)
                                       EDIT-CURSOR OF BI009(IX01)
                                       EDIT-CURSOR OF BI010(IX01)
                                       EDIT-CURSOR OF BI011(IX01)
                                       EDIT-CURSOR OF BI012(IX01)
                                       EDIT-CURSOR OF BI013(IX01)
                                       EDIT-CURSOR OF BI014(IX01)
              END-PERFORM
         WHEN      OTHER
              MOVE   10           TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   BODY-EXIT
     END-EVALUATE.
*入力項目 CHECK
     IF       HI001               =    1
              MOVE   "M"          TO   EDIT-OPTION  OF  HI003
              MOVE   " "          TO   EDIT-CURSOR  OF  HI003
              MOVE  HI003         TO   SOK-F01
              READ      ZSOKMS
                 INVALID
                    MOVE   11  TO  ERR-NO
                    MOVE  "R"  TO  EDIT-OPTION  OF  HI003
                    MOVE  "C"  TO  EDIT-CURSOR  OF  HI003
                 NOT  INVALID
                    MOVE   SOK-F02 TO   HO002
              END-READ
              IF   WK-DAISOKO     NOT =    "01"
                   IF   SOK-F01   NOT =     WK-JYO-SOKCD
                        MOVE   12   TO   ERR-NO
                        MOVE  "R"   TO   EDIT-OPTION  OF  HI003
                        MOVE  "C"   TO   EDIT-CURSOR  OF  HI003
                   END-IF
              END-IF
     END-IF.
* 明細入力有無チェック
     MOVE     SPACE          TO   WK-TOROKU.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    10
         IF      ( BI001    (IX01) NOT =    SPACE )   OR
                 ( BI002    (IX01) NOT =    SPACE )   OR
                 ( BI003    (IX01) NOT =    SPACE )   OR
                 ( BI004    (IX01) NOT =    SPACE )   OR
                 ( BI005    (IX01) NOT =    SPACE )   OR
                 ( BI006    (IX01) NOT =    SPACE )   OR
                 ( BI007    (IX01) NOT =    SPACE )   OR
                 ( BI008    (IX01) NOT =    SPACE )   OR
                 ( BI009    (IX01) NOT =    SPACE )   OR
                 ( BI010    (IX01) NUMERIC   AND
                   BI010    (IX01) NOT =    ZERO )    OR
                 ( BI011    (IX01) NUMERIC   AND
                   BI011    (IX01) NOT =    ZERO )    OR
                 ( BI012    (IX01) NUMERIC   AND
                   BI012    (IX01) NOT =    ZERO )    OR
                 ( BI013    (IX01) NOT =    SPACE )   OR
                 ( BI014    (IX01) NOT =    SPACE )
              PERFORM   BODY-CHECK-SEC
              MOVE   "1"          TO   WK-TOROKU
         ELSE
              MOVE   SPACE        TO   BODY01(IX01)
              MOVE   "M"          TO EDIT-OPTION OF BI001(IX01)
                                     EDIT-OPTION OF BI002(IX01)
                                     EDIT-OPTION OF BI003(IX01)
                                     EDIT-OPTION OF BI004(IX01)
                                     EDIT-OPTION OF BI005(IX01)
                                     EDIT-OPTION OF BI006(IX01)
                                     EDIT-OPTION OF BI007(IX01)
                                     EDIT-OPTION OF BI008(IX01)
                                     EDIT-OPTION OF BI009(IX01)
                                     EDIT-OPTION OF BI010(IX01)
                                     EDIT-OPTION OF BI011(IX01)
                                     EDIT-OPTION OF BI012(IX01)
                                     EDIT-OPTION OF BI013(IX01)
                                     EDIT-OPTION OF BI014(IX01)
              MOVE   " "          TO EDIT-CURSOR OF BI001(IX01)
                                     EDIT-CURSOR OF BI002(IX01)
                                     EDIT-CURSOR OF BI003(IX01)
                                     EDIT-CURSOR OF BI004(IX01)
                                     EDIT-CURSOR OF BI005(IX01)
                                     EDIT-CURSOR OF BI006(IX01)
                                     EDIT-CURSOR OF BI007(IX01)
                                     EDIT-CURSOR OF BI008(IX01)
                                     EDIT-CURSOR OF BI009(IX01)
                                     EDIT-CURSOR OF BI010(IX01)
                                     EDIT-CURSOR OF BI011(IX01)
                                     EDIT-CURSOR OF BI012(IX01)
                                     EDIT-CURSOR OF BI013(IX01)
                                     EDIT-CURSOR OF BI014(IX01)
         END-IF
     END-PERFORM.
*****重複チェック*****
* エラー無しで登録時、キー重複チェック
* 行未入力チェック
     IF  ( ERR-NO       =    ZERO )  AND
         ( HI001        =    1    )
           PERFORM    VARYING   IX01   FROM   1   BY   1
                      UNTIL     IX01    >     10
                  IF  ( BI001(IX01)   =   SPACE )  AND
                      ( BI002(IX01)   =   SPACE )  AND
                      ( BI003(IX01)   =   SPACE )  AND
                      ( BI004(IX01)   =   SPACE )  AND
                      ( BI005(IX01)   =   SPACE )  AND
                      ( BI006(IX01)   =   SPACE )  AND
                      ( BI007(IX01)   =   SPACE )
                        CONTINUE
                  ELSE
                       IF  (BI001(IX01)   NOT =   SPACE    OR
                            BI002(IX01)   NOT =   SPACE    OR
                            BI003(IX01)   NOT =   SPACE)       AND
                           (BI004(IX01)       =   SPACE   AND
                            BI005(IX01)       =   SPACE   AND
                            BI006(IX01)       =   SPACE   AND
                            BI007(IX01)       =   SPACE)
                            MOVE 19  TO ERR-NO
                            MOVE "R" TO EDIT-OPTION OF BI001(IX01)
                            MOVE "R" TO EDIT-OPTION OF BI002(IX01)
                            MOVE "R" TO EDIT-OPTION OF BI003(IX01)
                            MOVE "C" TO EDIT-CURSOR OF BI001(IX01)
                       ELSE
*************************** PERFORM    JYUFUKU-CHECK-SEC
                            CONTINUE
                       END-IF
                  END-IF
           END-PERFORM
     END-IF.
* 登録時、明細入力無しの時、エラー
     IF   WK-TOROKU   =   SPACE
          IF    ERR-NO   =   ZERO
                MOVE   20            TO   ERR-NO
          END-IF
     END-IF.
*********
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    ZERO
              MOVE  "OK"          TO   BODY-CHECK
              MOVE  "TAIL"        TO   SYORI-GROUP
     END-IF.
 BODY-EXIT.
     EXIT.
******************************************************************
*             ボディ部　チェック
******************************************************************
 BODY-CHECK-SEC         SECTION.
* 登録で明細データ有りの時、修正で削除フラグ＝９の時ＮＯチェック
     IF     ( HI001     =    1   AND   TB(IX01)  =   1 )  OR
            ( HI001          =    2    AND
              WK-F99(IX01)   =   "9"   AND
              BI013(IX01)    =   "9")
              GO                  TO   BODY-CHECK-EXIT
     END-IF.
* 新規登録フラグセット
     MOVE   "1"          TO   WK-TOROKU.
* 品単スペース右詰め
     MOVE     SPACE               TO   WK-BI005.
     PERFORM  VARYING   IX02      FROM      5    BY   -1
              UNTIL     IX02           <    1
         IF   BI005(IX01)(IX02:1) NOT  =    " "
              COMPUTE   IX02           =    6  -  IX02
              MOVE  BI005(IX01)        TO   WK-BI005 (IX02:)
              MOVE  WK-BI005           TO   BI005 (IX01)
              MOVE  1                  TO   IX02
         END-IF
     END-PERFORM.
* 登録時存在レコードのチェック（ＺＴＡＮＡＤＴ２により）
     IF       HI001                    =    1
              IF       TB(IX01)        =   ZERO
* 商品名称マスタ存在チェック
                  MOVE  BI004   (IX01)     TO   MEI-F011
                  MOVE  BI005   (IX01)     TO   MEI-F012 (1:5)
                  MOVE  BI006   (IX01)     TO   MEI-F012 (6:2)
                  MOVE  BI007   (IX01)     TO   MEI-F012 (8:1)
                  MOVE  ZERO               TO   INV-SW
                  READ      HMEIMS
                    INVALID
                       MOVE  SPACE         TO   BO002(IX01)
                       MOVE  1             TO   INV-SW
                       IF   ERR-NO         =    ZERO
                            MOVE   13      TO   ERR-NO
                       END-IF
                       MOVE  "R"   TO  EDIT-OPTION OF BI004 (IX01)
                       MOVE  "R"   TO  EDIT-OPTION OF BI005 (IX01)
                       MOVE  "R"   TO  EDIT-OPTION OF BI006 (IX01)
                       MOVE  "R"   TO  EDIT-OPTION OF BI007 (IX01)
                       MOVE  "C"   TO  EDIT-CURSOR OF BI004 (IX01)
                    NOT  INVALID
                       MOVE  MEI-F02       TO   SYOHIN-NAMEX
                       MOVE  SYOHIN-NAME   TO   BO002 (IX01)
                  END-READ
              END-IF
     END-IF.
* 削除フラグ入力チェック（修正時）
     IF       HI001                    =    2
              IF   BI013 (IX01)        =    " " OR  "9"
                   MOVE   "M"     TO   EDIT-OPTION OF BI013 (IX01)
                   MOVE   " "     TO   EDIT-CURSOR OF BI013 (IX01)
              ELSE
                   IF   ERR-NO         =    ZERO
                        MOVE 15   TO   ERR-NO
                   END-IF
                   MOVE   "R"     TO   EDIT-OPTION OF BI013 (IX01)
                   MOVE   "C"     TO   EDIT-CURSOR OF BI013 (IX01)
              END-IF
     END-IF.
*
 BODY-CHECK-EXIT.
     EXIT.
******************************************************************
*             ボディ部　重複チェック　　　　　
******************************************************************
 JYUFUKU-CHECK-SEC      SECTION.
*重複チェック（倉庫～品単）
     COMPUTE    IX03   =   IX01   +   1
     PERFORM    VARYING   IX03   FROM   IX03   BY   1
                                 UNTIL  IX03   >   10
            IF  ( BI001(IX01)  =  BI001(IX03) ) AND
                ( BI002(IX01)  =  BI002(IX03) ) AND
                ( BI003(IX01)  =  BI003(IX03) ) AND
                ( BI004(IX01)  =  BI004(IX03) ) AND
                ( BI005(IX01)  =  BI005(IX03) ) AND
                ( BI006(IX01)  =  BI006(IX03) ) AND
                ( BI007(IX01)  =  BI007(IX03) )
                   IF   ERR-NO  =  ZERO
                        MOVE   18  TO   ERR-NO
                   END-IF
                   MOVE "R" TO  EDIT-OPTION OF BI001(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI002(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI003(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI004(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI005(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI006(IX01)
                   MOVE "R" TO  EDIT-OPTION OF BI007(IX01)
                   MOVE "C" TO  EDIT-CURSOR OF BI001(IX01)
*                 ＩＸ０３行の反転
                   MOVE "R" TO  EDIT-OPTION OF BI001(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI002(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI003(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI004(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI005(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI006(IX03)
                   MOVE "R" TO  EDIT-OPTION OF BI007(IX03)
                   MOVE "C" TO  EDIT-CURSOR OF BI001(IX03)
            END-IF
     END-PERFORM.
 JYUFUKU-CHECK-EXIT.
     EXIT.
******************************************************************
*             アトリビュートクリア（ＢＯＤＹ）
******************************************************************
 BODY-ATTR-CLEAR        SECTION.
     MOVE   "M"                   TO   EDIT-OPTION  OF  HI003.
     MOVE   " "                   TO   EDIT-CURSOR  OF  HI003.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    10
         MOVE   SPACE             TO   BODY01 (IX01)
     END-PERFORM.
     PERFORM       VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    10
         MOVE  "M"                TO   EDIT-OPTION OF BI001 (IX01)
                                       EDIT-OPTION OF BI002 (IX01)
                                       EDIT-OPTION OF BI003 (IX01)
                                       EDIT-OPTION OF BI004 (IX01)
                                       EDIT-OPTION OF BI005 (IX01)
                                       EDIT-OPTION OF BI006 (IX01)
                                       EDIT-OPTION OF BI007 (IX01)
                                       EDIT-OPTION OF BI008 (IX01)
                                       EDIT-OPTION OF BI009 (IX01)
                                       EDIT-OPTION OF BI010 (IX01)
                                       EDIT-OPTION OF BI011 (IX01)
                                       EDIT-OPTION OF BI012 (IX01)
                                       EDIT-OPTION OF BI013 (IX01)
                                       EDIT-OPTION OF BI014 (IX01)
         MOVE  " "                TO   EDIT-CURSOR OF BI001 (IX01)
                                       EDIT-CURSOR OF BI002 (IX01)
                                       EDIT-CURSOR OF BI003 (IX01)
                                       EDIT-CURSOR OF BI004 (IX01)
                                       EDIT-CURSOR OF BI005 (IX01)
                                       EDIT-CURSOR OF BI006 (IX01)
                                       EDIT-CURSOR OF BI007 (IX01)
                                       EDIT-CURSOR OF BI008 (IX01)
                                       EDIT-CURSOR OF BI009 (IX01)
                                       EDIT-CURSOR OF BI010 (IX01)
                                       EDIT-CURSOR OF BI011 (IX01)
                                       EDIT-CURSOR OF BI012 (IX01)
                                       EDIT-CURSOR OF BI013 (IX01)
                                       EDIT-CURSOR OF BI014 (IX01)
     END-PERFORM.
 BODY-ATTR-EXIT.
     EXIT.
******************************************************************
*             テイル処理
******************************************************************
 TAIL-SEC               SECTION.
*確認入力
     MOVE     PMSG02              TO   MO002
     IF       HI001               =    1
              MOVE  "Y"           TO   TI001
     ELSE
              MOVE  "H"           TO   TI001
     END-IF.
     PERFORM            DSP-WR-SEC.
     MOVE    "TAIL"               TO   DSP-GRP.
     PERFORM            DSP-RD-SEC.
* アテンション判定
     EVALUATE    DSP-FNC
         WHEN     "F004"
              MOVE     SPACE      TO   FTA00301
              PERFORM   BODY-ATTR-CLEAR
              MOVE     "HEAD"     TO   SYORI-GROUP
                                  GO   TO   TAIL-EXIT
         WHEN     "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   TAIL-EXIT
         WHEN     "F006"
              PERFORM   TAIL-CLEAR-SEC
              PERFORM   ERR-DSP-SEC
              MOVE     "BODY"     TO   SYORI-GROUP
                                  GO   TO   TAIL-EXIT
         WHEN     "E000"
              CONTINUE
         WHEN      OTHER
              MOVE   10           TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   TAIL-EXIT
     END-EVALUATE.
* 確認Ｙ、Ｈ以外はエラー
     IF       TI001               =    "Y"  OR  "H"
              CONTINUE
     ELSE
              MOVE   7            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   TAIL-EXIT
     END-IF.
*　　_卸ファイル更新
     IF       BODY-CHECK          =   "OK"
         PERFORM   VARYING   IX01      FROM      1    BY   1
                   UNTIL     IX01           >    10
              IF   BI001    (IX01) NOT =    SPACE
              OR   BI002    (IX01) NOT =    SPACE
              OR   BI003    (IX01) NOT =    SPACE
              OR   BI004    (IX01) NOT =    SPACE
              OR   BI005    (IX01) NOT =    SPACE
              OR   BI006    (IX01) NOT =    SPACE
              OR   BI007    (IX01) NOT =    SPACE
                   PERFORM   TANA-FILE-SEC
              ELSE
                   PERFORM   TANA-CHECK-SEC
              END-IF
         END-PERFORM
     END-IF.
*確認”Ｙ”処理
     IF      ( HI001   =    1 )                           OR
             ( HI001   =    2   AND   TI001   =   "Y" )
              MOVE  SPACE         TO   FTA00301
              MOVE  "HEAD"        TO   SYORI-GROUP
                                  GO   TO   TAIL-EXIT
     END-IF.
*確認”Ｈ”処理
     MOVE     ZERO                TO   INV-SW    NOT-DELETE.
     MOVE     HI002               TO   TANA-F01.
     COMPUTE  TANA-F01            =   (TANA-F01  *  10) + 10.
     PERFORM            TANA-START-SEC.
*    _卸ファイルＳＴＡＲＴ　ＮＧ、ＡＴＥＮＤか本社端末の時
     IF     ( INV-SW             =   0   )    AND
            ( WK-DAISOKO         =  "01" )
              CONTINUE
     ELSE
*    本社以外の端末の時は自社分データのみ
              PERFORM  UNTIL   ( INV-SW    =    1 )   OR
                               ( TANA-F04  =    WK-JYO-SOKCD )
                        PERFORM        TANA-READ-SEC
              END-PERFORM
     END-IF.
*
     IF     ( INV-SW              =    0 )
              COMPUTE   HI002     =    TANA-F01  /  10
              PERFORM   BODY-SET-SEC
              IF   NOT-DELETE     =    0
                   MOVE  8        TO   ERR-NO
                   PERFORM   ERR-DSP-SEC
              ELSE
                   MOVE  SPACE    TO   MO001  TI001
                   PERFORM   DSP-WR-SEC
                   MOVE  "BODY"   TO   SYORI-GROUP
              END-IF
     ELSE
              MOVE   SPACE        TO   FTA00301
              MOVE   9            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
              MOVE  "HEAD"        TO   SYORI-GROUP
     END-IF.
 TAIL-EXIT.
     EXIT.
****************************************************************
*             _卸ファイル　処理
****************************************************************
 TANA-FILE-SEC          SECTION.
     IF      (HI001          =    1     AND
              TB(IX01)       =    1)       OR
             (HI001          =    2     AND
              WK-F99(IX01)   =   "9"    AND
              BI013(IX01)    =   "9")
         GO                            TO   TANA-FILE-EXIT
     END-IF.
     MOVE     ZERO                     TO   INV-SW.
     COMPUTE  TANA-F01  =   (HI002 * 10) + (IX01 - 1).
     READ     ZTANADT
         INVALID
              MOVE  1                  TO   INV-SW
     END-READ.
     IF       INV-SW                   =    1
              INITIALIZE                    TANA-REC
              COMPUTE   TANA-F01   =   (HI002 * 10) + (IX01 - 1)
              MOVE  ZERO               TO   TANA-F02
              MOVE  ZERO               TO   TANA-F03
              MOVE  SPACE              TO   TANA-F10
     END-IF.
     MOVE     HI003                    TO   TANA-F04.
     MOVE     BI004 (IX01)             TO   TANA-F05.
     MOVE     BI005 (IX01)             TO   TANA-F06.
     MOVE     BI006 (IX01)             TO   TANA-F07.
     MOVE     BI007 (IX01)             TO   TANA-F08.
     MOVE     BI008 (IX01)             TO   TANA-F09.
     MOVE     BI009 (IX01)             TO   TANA-F09 (5:1).
     IF       BI010 (IX01)                  NUMERIC
         MOVE  BI010 (IX01)            TO   TANA-F11
     ELSE
         MOVE  ZERO                    TO   TANA-F11
     END-IF.
     IF       BI011 (IX01)                  NUMERIC
         MOVE  BI011 (IX01)            TO   TANA-F12
     ELSE
         MOVE  ZERO                    TO   TANA-F12
     END-IF.
     IF       BI012 (IX01)                  NUMERIC
         MOVE  BI012 (IX01)            TO   TANA-F13
     ELSE
         MOVE  ZERO                    TO   TANA-F13
     END-IF.
     MOVE     BI014 (IX01)             TO   TANA-F14.
     MOVE     BI001 (IX01)             TO   TANA-F15(1:1).
     MOVE     BI002 (IX01)             TO   TANA-F15(2:3).
     MOVE     BI003 (IX01)             TO   TANA-F15(5:2).
     MOVE     BO001 (IX01)             TO   TANA-F16.
     MOVE     BI013 (IX01)             TO   TANA-F99.
     IF       INV-SW                   =    1
              MOVE      ZERO           TO   TANA-F16
              WRITE     TANA-REC
     ELSE
              REWRITE   TANA-REC
     END-IF.
 TANA-FILE-EXIT.
     EXIT.
****************************************************************
*             _卸ファイル　チェック
****************************************************************
 TANA-CHECK-SEC         SECTION.
     COMPUTE  TANA-F01  =   (HI002 * 10) + (IX01 - 1).
     READ     ZTANADT
         INVALID
              CONTINUE
         NOT  INVALID
              DELETE    ZTANADT
     END-READ.
 TANA-CHECK-EXIT.
     EXIT.
****************************************************************
*             ボディ部　クリア
****************************************************************
 BODY-CLEAR-SEC         SECTION.
     MOVE     SPACE               TO   BODYG.
 BODY-CLEAR-EXIT.
     EXIT.
****************************************************************
*             テイル部　クリア
****************************************************************
 TAIL-CLEAR-SEC         SECTION.
     MOVE     SPACE               TO   TI001.
 TAIL-CLEAR-EXIT.
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
 ERR-DSP-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE         ZTANADT
                   ZTANADT2
                   HJYOKEN
                   HMEIMS
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
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
*
     MOVE    "SCREEN"             TO   DSP-GRP
     MOVE     SPACE               TO   DSP-PRO.
     WRITE    FTA00301.
 DSP-WR-EXIT.
     EXIT.

```
