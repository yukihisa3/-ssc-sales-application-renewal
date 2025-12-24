***************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿             *
*    業務名　　　　　　　：　実績管理システム                 *
*    モジュール名　　　　：　受払い照会（相手商品CD）
*    作成日／更新日　　　：　2011/10/17                       *
*    処理概要　　　　　　：　商品の受払いを照会する。         *
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SJS0102I.
 AUTHOR.                   S.I.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-6500.
 OBJECT-COMPUTER.          K-6500.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS
     STATION     IS        STA.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*実績累積ファイル
     SELECT      RUISEKL  ASSIGN    TO         DA-01-VI-RUISEKL6
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY
                             RUI-F23 *> 相手商品ＣＤ
                             RUI-F17 *> 場所
                             RUI-F09 *> 出荷日
                           FILE      STATUS    RUI-ST   RUI-ST1.
*受払抽出ファイル
     SELECT      UKEBWKL1  ASSIGN    TO        DA-01-VI-UKEBWKL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       UKE-F01
                           FILE      STATUS    UKE-ST   UKE-ST1.
*商品変換テーブル
     SELECT      SHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL7
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY
                             SHT-F02  *> 相手商品ＣＤ
                           FILE      STATUS    SHT-ST    SHT-ST1.
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
*作業区分マスタ
     SELECT      SGYKBMF   ASSIGN    TO        DA-01-VI-SGYKBML1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       KBM-F01
                           FILE      STATUS    KBM-ST    KBM-ST1.
*取引先マスタ(2005/09/15 追加)
     SELECT      TOKMS3    ASSIGN    TO        DA-01-VI-TOKMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST    TOK-ST1.
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
*実績累積ファイル
 FD  RUISEKL.
     COPY        RUISEKF   OF        XFDLIB
     JOINING     RUI       AS        PREFIX.
*受払抽出ファイル
 FD  UKEBWKL1.
     COPY        UKEBWKF   OF        XFDLIB
     JOINING     UKE       AS        PREFIX.
*商品変換ＴＢＬ
 FD  SHOTBL.
     COPY        SHOTBL4   OF        XFDLIB
     JOINING     SHT       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYOKEN    AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*作業区分マスタ
 FD  SGYKBMF.
     COPY        SGYKBMF   OF        XFDLIB
     JOINING     KBM       AS        PREFIX.
*取引先マスタ(2005/09/15 追加)
 FD  TOKMS3.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FJS01021  OF        XMDLIB
     JOINING     DSP       AS        PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
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
     03  MSG01              PIC  N(20) VALUE
         NC"無効ＰＦキーです".
     03  MSG02              PIC  N(20) VALUE
         NC"　".
     03  MSG03              PIC  N(20) VALUE
         NC"　".
     03  MSG04              PIC  N(20) VALUE
         NC"　".
     03  MSG05              PIC  N(20) VALUE
         NC"　".
     03  MSG06              PIC  N(20) VALUE
         NC"前頁はありません".
     03  MSG07              PIC  N(20) VALUE
         NC"次頁はありません".
     03  MSG08              PIC  N(20) VALUE
         NC"前レコードはありません".
     03  MSG09              PIC  N(20) VALUE
         NC"次レコードはありません".
     03  MSG10              PIC  N(20) VALUE
         NC"倉庫マスタに存在しません".
     03  MSG11              PIC  N(20) VALUE
         NC"指定されたレコードは存在しません".
     03  MSG12              PIC  N(20) VALUE
         NC"相手商品コードを入力して下さい".
     03  MSG13              PIC  N(20) VALUE
         NC"　".
     03  MSG14              PIC  N(20) VALUE
         NC"場所を入力して下さい".
     03  MSG15              PIC  N(20) VALUE
         NC"日付に誤りがあります".
     03  MSG16              PIC  N(20) VALUE
         NC"商品変換ＴＢＬに未登録です".
     03  MSG17              PIC  N(20) VALUE
         NC"　".
     03  MSG18              PIC  N(20) VALUE
         NC"　".
     03  MSG19              PIC  N(20) VALUE
         NC"　".
     03  MSG20              PIC  N(20) VALUE
         NC"　".
     03  MSG21              PIC  N(20) VALUE
         NC"　".
     03  MSG22              PIC  N(20) VALUE
         NC"　".
     03  MSG23              PIC  N(20) VALUE
         NC"　".
     03  MSG24              PIC  N(20) VALUE
         NC"　".
     03  MSG25              PIC  N(20) VALUE
         NC"　".
     03  MSG26              PIC  N(20) VALUE
         NC"　".
     03  MSG27              PIC  N(20) VALUE
         NC"　".
     03  MSG28              PIC  N(20) VALUE
         NC"　".
     03  MSG29              PIC  N(20) VALUE
         NC"　".
     03  MSG30              PIC  N(20) VALUE
         NC"　".
 01  MSG-AREAR  REDEFINES MSG-AREA.
     03  MSGIX              PIC  N(20)  OCCURS 30.
*
 01  PFKEY-MSG.
     03  PMSG01            PIC N(30) VALUE
       NC"_取消　_終了　_前レコード　_次レコード".
     03  PMSG02            PIC N(30) VALUE
       NC"_取消　_終了　_項目戻り　_前頁　_次頁".
     03  PMSG02R.
       05  PMSG02A         PIC N(08).
       05  PMSG02IX        PIC N(01) OCCURS 22.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
****  商品右詰め用エリア      ****
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)   OCCURS  8.
*
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
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  SYORI-GROUP       PIC X(04).
     03  WK-MEI-F02G.
       05  WK-MEI-F02      PIC  N(30).
     03  ERR-NO            PIC  9(02)  VALUE  ZERO.
     03  WK-HIZUKE         PIC  9(08)  VALUE  ZERO.
     03  FG-SHOTBL-INV     PIC  9(01).
     03  FG-ZSOKMS-INV     PIC  9(01).

 01  CNT-AREA.
     03  CNT-SEQ           PIC  9(08)  VALUE  ZERO.
     03  WK-SEQ            PIC  9(08)  VALUE  ZERO.
     03  CNT-PAGE          PIC  9(08)  VALUE  ZERO.
     03  CNT-UKEBWKF       PIC  9(08)  VALUE  ZERO.
     03  MAX-PAGE          PIC  9(08)  VALUE  ZERO.
     03  WK-AMARI          PIC  9(02)  VALUE  ZERO.
     03  FG-RUISEKF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-UKEBWKF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-FWD-BWD         PIC  X(03).
     03  MEIMS-FLG         PIC  X(03)  VALUE  SPACE.
     03  RUI-REC-INV       PIC  X(03)  VALUE  SPACE.
     03  IX1               PIC  9(02)  VALUE  ZERO.
     03  IXA               PIC  9(02)  VALUE  ZERO.
 01  WK-HINT.
     03  WK-HIN1           PIC  X(05)  VALUE  SPACE.
     03  WK-HIN2           PIC  X(02)  VALUE  SPACE.
     03  WK-HIN3           PIC  X(01)  VALUE  SPACE.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  RUI-ST            PIC X(02).
     03  RUI-ST1           PIC X(04).
     03  UKE-ST            PIC X(02).
     03  UKE-ST1           PIC X(04).
     03  JYKN-ST           PIC X(02).
     03  JYKN-ST1          PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
     03  KBM-ST            PIC X(02).
     03  KBM-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SHT-ST            PIC X(02).
     03  SHT-ST1           PIC X(04).
*
     03  FILE-ERR1         PIC N(20) VALUE
                     NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(20) VALUE
                     NC"実績累計ファイル異常！".
     03  FILE-ERR3         PIC N(20) VALUE
                     NC"受払抽出ファイル異常！".
     03  FILE-ERR4         PIC N(20) VALUE
                     NC"商品名称マスタ異常！".
     03  FILE-ERR5         PIC N(20) VALUE
                     NC"条件ファイル異常！".
     03  FILE-ERR6         PIC N(20) VALUE
                     NC"倉庫マスタ異常！".
     03  FILE-ERR7         PIC N(20) VALUE
                     NC"作業区分マスタ異常！".
     03  FILE-ERR8         PIC N(20) VALUE
                     NC"取引先マスタ異常！".
     03  FILE-ERR9         PIC N(20) VALUE
                     NC"商品変換ＴＢＬ異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 LINKAGE                SECTION.
******************************************************************
 01  LINK-SOKCD            PIC X(02). *> 倉庫ＣＤ
 01  LINK-DSOKCD           PIC X(02). *> 代表倉庫ＣＤ
******************************************************************
 PROCEDURE              DIVISION       USING     LINK-SOKCD
                                                 LINK-DSOKCD.
******************************************************************
*             ファイルエラー処理
******************************************************************
 DECLARATIVES.
 RUI-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         RUISEKL.
     DISPLAY       RUI-ST    UPON      STA.
     DISPLAY       RUI-ST1   UPON      STA.
     DISPLAY       FILE-ERR2 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 UKE-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         UKEBWKL1.
     DISPLAY       UKE-ST    UPON      STA.
     DISPLAY       UKE-ST1   UPON      STA.
     DISPLAY       FILE-ERR3 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 SHOTBL-ERR             SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         SHOTBL.
     DISPLAY       SHT-ST    UPON      STA.
     DISPLAY       SHT-ST1   UPON      STA.
     DISPLAY       FILE-ERR9 UPON      STA.
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
 SOK-KBM                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         SGYKBMF.
     DISPLAY       KBM-ST    UPON      STA.
     DISPLAY       KBM-ST1   UPON      STA.
     DISPLAY       FILE-ERR7 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
*----<2005/09/15 追加>----
 TOK-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         TOKMS3.
     DISPLAY       TOK-ST    UPON      STA.
     DISPLAY       TOK-ST1   UPON      STA.
     DISPLAY       FILE-ERR8 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
*-------------------------
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
 SHORI-SEC             SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = 9.
     PERFORM  END-SEC.
     STOP RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*             初期処理
******************************************************************
 INIT-SEC              SECTION.
     OPEN  INPUT RUISEKL
                 SHOTBL
                 HJYOKEN
                 ZSOKMS
                 SGYKBMF
                 TOKMS3
           I-O   DSPFILE.

     MOVE  "HEAD"           TO  SYORI-GROUP.
     PERFORM  DSP-INIT-SEC.

 INIT-EXIT.
     EXIT.
******************************************************************
*             初期画面　表示
******************************************************************
 DSP-INIT-SEC           SECTION.
     IF  SYORI-GROUP = "HEAD"
         MOVE  SPACE        TO  DSP-FJS01021
     ELSE
         MOVE  SPACE        TO  DSP-BODYG
     END-IF.

* 特販部名称編集
     MOVE  SPACE            TO  JYOKEN-REC.
     INITIALIZE                 JYOKEN-REC.
     MOVE  "99"             TO  JYOKEN-F01.
     MOVE  "BUMON"          TO  JYOKEN-F02.
     READ  HJYOKEN
       INVALID KEY
         MOVE ALL NC"＊"    TO  HEN-TOKHAN
       NOT INVALID KEY
         MOVE JYOKEN-F03    TO  HEN-TOKHAN
     END-READ.

* 倉庫ＣＤの初期表示
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         MOVE LINK-SOKCD    TO  DSP-SOKCD  SOK-F01
         READ  ZSOKMS
           INVALID
             CONTINUE
           NOT  INVALID
             MOVE  SOK-F02  TO  DSP-SOKNM
         END-READ

         MOVE  "X"  TO  EDIT-STATUS OF DSP-SOKCD
     END-IF.

     PERFORM  DSP-ATTRHD-INIT-SEC.

 DSP-INIT-EXIT.
     EXIT.
******************************************************************
*  ヘッダ入力項目属性初期化
******************************************************************
 DSP-ATTRHD-INIT-SEC              SECTION.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-AITSHO.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-AITSHO.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-SOKCD.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SOKCD.
     MOVE  "M"    TO  EDIT-OPTION OF DSP-HIZUKE.
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HIZUKE.
 DSP-ATTRHD-INIT-EXIT.
     EXIT.
******************************************************************
*             主処理
******************************************************************
 MAIN-SEC              SECTION.
     EVALUATE  SYORI-GROUP

       WHEN  "HEAD"
         PERFORM  HEAD-SEC

       WHEN  "BODY"
         PERFORM  BODY-SEC

     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
******************************************************************
*             ヘッド処理
******************************************************************
 HEAD-SEC               SECTION.
     PERFORM  DSP-WR-SEC.
     PERFORM  DSP-RD-SEC.

     EVALUATE  DSP-FNC
       WHEN  "E000"
         PERFORM  HEADB-SEC

       WHEN  "F004"
         PERFORM  DSP-INIT-SEC

       WHEN  "F005"
         MOVE  9        TO  END-FLG

       WHEN  "F007"
         PERFORM  BWD-REC-SEC

       WHEN  "F008"
         PERFORM  FWD-REC-SEC

       WHEN  OTHER
         MOVE  1        TO  ERR-NO

     END-EVALUATE.

 HEAD-EXIT.
     EXIT.
****************************************************************
*             画面    ＷＲＩＴＥ
****************************************************************
 DSP-WR-SEC            SECTION.
* システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING  LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
* 画面表示日付編集
     MOVE  SYS-DATE (1:4)   TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE (5:2)   TO  HEN-DATE-MM.
     MOVE  SYS-DATE (7:2)   TO  HEN-DATE-DD.
* システム日付取得
     ACCEPT  WK-TIME   FROM TIME.
* 画面表示時刻編集
     MOVE  WK-TIME (1:2)    TO  HEN-TIME-HH.
     MOVE  WK-TIME (3:2)    TO  HEN-TIME-MM.
     MOVE  WK-TIME (5:2)    TO  HEN-TIME-SS.

     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.
     MOVE  HEN-TOKHAN-AREA  TO  DSP-TOKHAN.

* ＰＦキーガイド編集
     IF  SYORI-GROUP = "HEAD"
         MOVE  PMSG01       TO  DSP-MO002
     ELSE
         MOVE  PMSG02       TO  DSP-MO002
     END-IF.

* メッセージ編集
     IF  ERR-NO = ZERO
         MOVE  SPACE           TO  DSP-MO001
     ELSE
         MOVE  MSGIX (ERR-NO)  TO  DSP-MO001
     END-IF.

     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FJS01021"       TO  DSP-FMT.
     MOVE  SPACE            TO  DSP-PRO.

     WRITE  DSP-FJS01021.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*             画面　　ＲＥＡＤ
****************************************************************
 DSP-RD-SEC            SECTION.

     IF  SYORI-GROUP = "HEAD"
         MOVE  "HEAD"       TO  DSP-GRP
     ELSE
         MOVE  "BODY"       TO  DSP-GRP
     END-IF.

     MOVE  "FJS01021"       TO  DSP-FMT.
     MOVE  "NE"             TO  DSP-PRO.

     READ  DSPFILE.

     MOVE  ZERO             TO  ERR-NO.
 DSP-RD-EXIT.
     EXIT.
******************************************************************
*             ヘッドＢ処理
******************************************************************
 HEADB-SEC               SECTION.
* 属性の初期化
     PERFORM  DSP-ATTRHD-INIT-SEC.

* 相手商品コード
     IF  DSP-AITSHO = SPACE
         IF ERR-NO = ZERO
            MOVE  12        TO  ERR-NO
         END-IF
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"  TO  EDIT-OPTION OF DSP-AITSHO
         GO TO  HEAD-AITSHO-EXIT
     END-IF.

     MOVE  DSP-AITSHO       TO  SHT-F02 *> 相手取引先ＣＤ
     PERFORM  SHOTBL-RD-SEC.
     IF FG-SHOTBL-INV = 1
        IF ERR-NO = ZERO
           MOVE  16         TO  ERR-NO
        END-IF
        MOVE  "C"  TO  EDIT-CURSOR OF DSP-AITSHO
        MOVE  "R"  TO  EDIT-OPTION OF DSP-AITSHO
        GO TO  HEAD-AITSHO-EXIT
     END-IF.

 HEAD-AITSHO-EXIT.
* 倉庫コード
     MOVE  SPACE            TO  DSP-SOKNM.

     IF  LINK-DSOKCD  NOT =  "01" *> 代表倉庫ＣＤ
         *> 画面倉庫ＣＤは初期表示されプロテクトがか
         *> かっているためチェックしない。
         MOVE  DSP-SOKCD    TO  SOK-F01
         PERFORM  RD-ZSOKMS-SEC
         IF  FG-ZSOKMS-INV = ZERO
             MOVE SOK-F02   TO  DSP-SOKNM
         END-IF
         GO TO  HEAD-SOKCD-EXIT
     END-IF.

     IF  DSP-SOKCD  = SPACE
         IF  ERR-NO = ZERO
             MOVE  14       TO  ERR-NO
         END-IF
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
         GO TO  HEAD-SOKCD-EXIT
     END-IF.

     MOVE  DSP-SOKCD        TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = 1
         IF  ERR-NO = ZERO
             MOVE  10       TO  ERR-NO
         END-IF
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD

     ELSE
         MOVE SOK-F02       TO  DSP-SOKNM
     END-IF.

 HEAD-SOKCD-EXIT.
* 処理日
     IF      DSP-HIZUKE NUMERIC
         AND DSP-HIZUKE > ZERO
         CONTINUE
     ELSE
         MOVE  SPACE        TO  DSP-HIZUKE(1:)
         MOVE  ZERO         TO  WK-HIZUKE
         GO TO  HEAD-HIZUKE-EXIT
     END-IF.

     MOVE  "3"              TO  LINK-IN-KBN
     MOVE  DSP-HIZUKE       TO  LINK-IN-YMD6
     MOVE  ZERO             TO  LINK-IN-YMD8
     MOVE  ZERO             TO  LINK-OUT-RET
     MOVE  ZERO             TO  LINK-OUT-YMD
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD
     IF  LINK-OUT-RET NOT =  ZERO
         IF ERR-NO = ZERO
            MOVE  15        TO  ERR-NO
         END-IF
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-HIZUKE
         MOVE  "R"  TO  EDIT-OPTION OF DSP-HIZUKE
         GO TO  HEAD-HIZUKE-EXIT
     END-IF.

     MOVE  LINK-OUT-YMD     TO  WK-HIZUKE.

 HEAD-HIZUKE-EXIT.
     IF  ERR-NO NOT = ZERO
         GO TO  HEAD-EXIT
     END-IF.

* 受払抽出ファイル出力
     PERFORM  UKEBWKF-WRITE-SEC.
     IF  CNT-UKEBWKF = ZERO *>対象データなし
         IF  ERR-NO = ZERO
             MOVE  11       TO  ERR-NO
         END-IF

         MOVE  "C"  TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"  TO  EDIT-OPTION OF DSP-AITSHO
         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
         MOVE  "R"  TO  EDIT-OPTION OF DSP-HIZUKE
         GO TO  HEAD-EXIT
     END-IF.

* 最大ページを計算する。
     IF  CNT-UKEBWKF <= 14
         MOVE  1            TO  MAX-PAGE
     ELSE
         DIVIDE  CNT-UKEBWKF BY 14
                 GIVING     MAX-PAGE
                 REMAINDER  WK-AMARI
         IF  WK-AMARI > ZERO
             ADD  1  TO  MAX-PAGE
         END-IF
     END-IF.

* 画面編集（明細）
     MOVE  1                TO  CNT-PAGE.
     PERFORM  BODY-SET-SEC.

     IF  ERR-NO NOT = ZERO
         GO TO  HEAD-EXIT
     END-IF.

     MOVE  "BODY"           TO  SYORI-GROUP.

 HEADB-EXIT.
     EXIT.
******************************************************************
*    商品変換ＴＢＬ検索処理
******************************************************************
 SHOTBL-RD-SEC              SECTION.
     MOVE  ZERO             TO  FG-SHOTBL-INV.

     START  SHOTBL  KEY >= SHT-F02
       INVALID
         MOVE  1            TO  FG-SHOTBL-INV
         GO TO  SHOTBL-RD-EXIT
     END-START.

     READ  SHOTBL
       AT END
         MOVE  1            TO  FG-SHOTBL-INV
         GO TO  SHOTBL-RD-EXIT
     END-READ.

     IF  SHT-F02 NOT = DSP-AITSHO  *> 相手取引先ＣＤ
         MOVE  1            TO  FG-SHOTBL-INV
         GO TO  SHOTBL-RD-EXIT
     END-IF.

 SHOTBL-RD-EXIT.
     EXIT.
******************************************************************
*  倉庫マスタ検索
******************************************************************
 RD-ZSOKMS-SEC               SECTION.
     READ  ZSOKMS
       INVALID
         MOVE  1            TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZSOKMS-INV
     END-READ.
 RD-ZSOKMSEXIT.
     EXIT.
******************************************************************
*             受払抽出ファイル出力処理
******************************************************************
 UKEBWKF-WRITE-SEC      SECTION.
     MOVE  "UKEBWKF-WRITE-SEC"   TO  DSP-GRP.

     OPEN  OUTPUT UKEBWKL1.
     MOVE  SPACE            TO  FG-RUISEKF-END.
     MOVE  ZERO             TO  CNT-SEQ.
     MOVE  ZERO             TO  CNT-UKEBWKF.

     MOVE  SPACE            TO  RUI-REC.
     INITIALIZE  RUI-REC.

     MOVE  DSP-AITSHO       TO  RUI-F23. *>相手商品CD
     MOVE  DSP-SOKCD        TO  RUI-F17. *>場所
     MOVE  WK-HIZUKE        TO  RUI-F09. *>出荷日

     MOVE  LOW-VALUE        TO  FG-RUISEKF-END.
     PERFORM  RUI-READ-SEC.

     PERFORM  UNTIL FG-RUISEKF-END = "END"
       PERFORM  UKEBWKF-WRITEB-SEC
       PERFORM  RUI-READ-SEC
     END-PERFORM.

     CLOSE  UKEBWKL1.
 UKEBWKF-WRITE-EXIT.
     EXIT.
******************************************************************
*             実績累積ファイル読み込み
******************************************************************
 RUI-READ-SEC           SECTION.

     IF  FG-RUISEKF-END = LOW-VALUE
         MOVE  SPACE        TO  FG-RUISEKF-END
         START  RUISEKL  KEY >= RUI-F23
                                RUI-F17
                                RUI-F09
           INVALID KEY
             MOVE  "END"    TO  FG-RUISEKF-END
             GO TO  RUI-READ-EXIT
         END-START

     END-IF.

     READ  RUISEKL NEXT
       AT END
         MOVE  "END"        TO  FG-RUISEKF-END
         GO TO  RUI-READ-EXIT
     END-READ.

     IF      RUI-F23 = DSP-AITSHO *> 相手商品CD
         AND RUI-F17 = DSP-SOKCD  *> 場所
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-RUISEKF-END
         GO TO  RUI-READ-EXIT
     END-IF.

     IF  WK-HIZUKE = ZERO
         CONTINUE
     ELSE
         IF  RUI-F09 = WK-HIZUKE  *> 出荷日
             CONTINUE
         ELSE
             MOVE  "END"    TO  FG-RUISEKF-END
             GO TO  RUI-READ-EXIT
         END-IF
     END-IF.

     IF  RUI-F02 = "30" OR "31" OR "32" OR "40" OR
                   "41" OR "50" OR "51" OR "70" OR
                   "71" OR "60" OR "42"
         CONTINUE
     ELSE
         GO TO  RUI-READ-SEC
     END-IF.

 RUI-READ-EXIT.
     EXIT.
******************************************************************
*  受払抽出ファイル出力Ｂ処理
******************************************************************
 UKEBWKF-WRITEB-SEC           SECTION.

     MOVE  SPACE            TO  UKE-REC.
     INITIALIZE  UKE-REC.

     ADD  1   TO  CNT-SEQ.
     MOVE  CNT-SEQ          TO  UKE-F01.
     MOVE  RUI-F09          TO  UKE-F02.
     MOVE  RUI-F03          TO  UKE-F03.
     MOVE  RUI-F02          TO  UKE-F04.
     MOVE  RUI-F16          TO  UKE-F05.
     MOVE  RUI-F06          TO  UKE-F09.
     MOVE  RUI-F13          TO  UKE-F06.
     MOVE  RUI-F22          TO  UKE-F07.
     MOVE  RUI-F05          TO  UKE-F08.
     MOVE  RUI-F27          TO  UKE-F99. *>担当者ＣＤ
     WRITE  UKE-REC.
     ADD  1   TO  CNT-UKEBWKF.

 UKEBWKF-WRITEB-EXIT.
     EXIT.
******************************************************************
*  ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.

     OPEN  INPUT UKEBWKL1. *>受払抽出ファイル
     IF  CNT-PAGE > MAX-PAGE
         GO TO  BODY-SET-090
     END-IF.

     MOVE  SPACE            TO  DSP-BODYG.
     COMPUTE  WK-SEQ = CNT-PAGE * 14 - 13.

     MOVE  LOW-VALUE        TO  FG-UKEBWKF-END.
     MOVE  WK-SEQ           TO  UKE-F01.
     PERFORM  UKEBWKL1-RD-SEC.

     PERFORM  VARYING IX1  FROM 1 BY 1
              UNTIL   IX1 > 14
                   OR FG-UKEBWKF-END = "END"
       PERFORM  BODY-SETB-SEC
       PERFORM  UKEBWKL1-RD-SEC

     END-PERFORM.

 BODY-SET-090.
     CLOSE  UKEBWKL1. *>受払抽出ファイル

 BODY-SET-EXIT.
     EXIT.
************************************************************
*  受払抽出ファイル読込み処理
************************************************************
 UKEBWKL1-RD-SEC              SECTION.
     IF FG-UKEBWKF-END = LOW-VALUE
        MOVE  SPACE         TO  FG-UKEBWKF-END
        START  UKEBWKL1 KEY >= UKE-F01
          INVALID
            GO TO  UKEBWKL1-RD-EXIT
        END-START
     END-IF.

     READ  UKEBWKL1 NEXT
       AT END
         MOVE  "END"        TO  FG-UKEBWKF-END
         GO TO  UKEBWKL1-RD-EXIT
     END-READ.

 UKEBWKL1-RD-EXIT.
     EXIT.
************************************************************
*             ボディ部　セットＢ処理
************************************************************
 BODY-SETB-SEC              SECTION.

     MOVE  UKE-F02(3:6)     TO  DSP-SYUYMD(IX1).
*    入出庫事由
     IF  UKE-F04 = "40" OR "41" OR "50" OR "51" OR
                   "70" OR "71" OR "42"
         MOVE  SPACE        TO  JYOKEN-REC
         INITIALIZE             JYOKEN-REC
         MOVE  1            TO  JYOKEN-F01
         MOVE  UKE-F04      TO  JYOKEN-F02
         READ  HJYOKEN
           INVALID KEY
             MOVE  ALL NC"＊"    TO  DSP-JIYU (IX1)
           NOT INVALID KEY
             MOVE  JYOKEN-F03    TO  DSP-JIYU (IX1)
         END-READ
     ELSE
         MOVE  SPACE        TO  KBM-REC
         INITIALIZE  KBM-REC
         MOVE  UKE-F07      TO  KBM-F01
         READ  SGYKBMF
           INVALID KEY
             MOVE  ALL NC"＊"    TO  DSP-JIYU (IX1)
           NOT INVALID KEY
             MOVE  KBM-F02       TO  DSP-JIYU (IX1)
         END-READ
     END-IF.

     MOVE  UKE-F03          TO  DSP-DENNO (IX1)
     EVALUATE  UKE-F05
       WHEN  "1"
         MOVE  NC"入"       TO  DSP-IRIDE (IX1)
       WHEN  "2"
         MOVE  NC"出"       TO  DSP-IRIDE (IX1)
       WHEN  "3"
         MOVE  NC"発"       TO  DSP-IRIDE (IX1)
       WHEN  OTHER
         MOVE  NC"＊"       TO  DSP-IRIDE (IX1)
     END-EVALUATE.

     IF  UKE-F08 = 1 OR 3 OR 5 OR 7 OR 9
         COMPUTE  DSP-UKESUU (IX1) =  UKE-F06 * -1
     ELSE
         MOVE  UKE-F06      TO  DSP-UKESUU (IX1)
     END-IF.

*得意先名取得
     IF  UKE-F04 = "40" OR "41" OR "42"
         MOVE  SPACE        TO  TOK-REC
         INITIALIZE  TOK-REC
         MOVE  UKE-F09      TO  TOK-F01
         READ  TOKMS3
           INVALID KEY
             MOVE  ALL NC"＊"    TO  DSP-TOKNM (IX1)
           NOT INVALID KEY
             MOVE  TOK-F03(1:8)  TO  DSP-TOKNM (IX1)
         END-READ
     END-IF.

*担当
     MOVE  UKE-F99          TO  DSP-TANTO (IX1).

 BODY-SETB-EXIT.
     EXIT.
******************************************************************
*  前レコード処理
******************************************************************
 BWD-REC-SEC           SECTION.

     MOVE  SPACE            TO  DSP-BODYG.
* 属性の初期化
     PERFORM  DSP-ATTRHD-INIT-SEC.

     MOVE  SPACE            TO  RUI-REC.
     INITIALIZE  RUI-REC.

     IF  LINK-DSOKCD = "01"
         MOVE  DSP-AITSHO   TO  RUI-F23 *>相手商品CD
         MOVE  DSP-SOKCD    TO  RUI-F17 *>場所
         MOVE  ZERO         TO  RUI-F09 *>出荷日
     ELSE
         *> 初期表示で入力不可なので相手先商品ＣＤで移動
         MOVE  DSP-AITSHO   TO  RUI-F23 *>相手商品CD
         MOVE  LOW-VALUE    TO  RUI-F17 *>場所
         MOVE  ZERO         TO  RUI-F09 *>出荷日
     END-IF.

     MOVE  "BWD"            TO  FG-FWD-BWD.
     MOVE  LOW-VALUE        TO  FG-RUISEKF-END.
     PERFORM  RUI-READ2-SEC.
     IF  FG-RUISEKF-END = "END"
         IF  ERR-NO = ZERO
             MOVE   8       TO  ERR-NO
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SOKCD
         GO TO  BWD-REC-EXIT
     END-IF.

     MOVE  RUI-F23          TO  DSP-AITSHO.
     IF  LINK-DSOKCD = "01"
         MOVE  RUI-F17      TO  DSP-SOKCD
     ELSE
         *> 初期表示で入力不可なのでそのまま
         CONTINUE
     END-IF.

     IF  DSP-SOKCD NOT = SPACE
         MOVE  DSP-SOKCD    TO  SOK-F01
         PERFORM  RD-ZSOKMS-SEC
         IF  FG-ZSOKMS-INV = 1
             MOVE  SPACE    TO  DSP-SOKNM
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKNM
         END-IF
     ELSE
         MOVE  SPACE        TO  DSP-SOKNM
     END-IF.

     MOVE  SPACE            TO  DSP-HIZUKE(1:).

 BWD-REC-EXIT.
     EXIT.
******************************************************************
*  実績累積ファイル読み込み２
******************************************************************
 RUI-READ2-SEC         SECTION.

     IF  FG-RUISEKF-END = LOW-VALUE
         MOVE  SPACE        TO  FG-RUISEKF-END
         IF  FG-FWD-BWD = "FWD" *> 順読み
             START  RUISEKL  KEY > RUI-F23
                                   RUI-F17
                                   RUI-F09
               INVALID KEY
                 MOVE  "END"    TO  FG-RUISEKF-END
                 GO TO  RUI-READ2-EXIT
             END-START
         ELSE                   *> 逆読み
             MOVE  SPACE        TO  FG-RUISEKF-END
             START  RUISEKL  KEY < RUI-F23
                                   RUI-F17
                                   RUI-F09
                    WITH REVERSED ORDER
               INVALID KEY
                 MOVE  "END"    TO  FG-RUISEKF-END
                 GO TO  RUI-READ2-EXIT
             END-START
         END-IF
     END-IF.

     READ  RUISEKL NEXT
       AT END
         MOVE  "END"        TO  FG-RUISEKF-END
         GO TO  RUI-READ2-EXIT
     END-READ.

     IF  RUI-F02 = "30" OR "31" OR "32" OR "40" OR
                   "41" OR "50" OR "51" OR "70" OR
                   "71" OR "60" OR "42"
         CONTINUE
     ELSE
         GO TO  RUI-READ2-SEC
     END-IF.

 RUI-READ2-EXIT.
     EXIT.
******************************************************************
*  次レコード処理
******************************************************************
 FWD-REC-SEC           SECTION.

     MOVE  SPACE            TO  DSP-BODYG.
* 属性の初期化
     PERFORM  DSP-ATTRHD-INIT-SEC.

     MOVE  SPACE            TO  RUI-REC.
     INITIALIZE  RUI-REC.

     IF  LINK-DSOKCD = "01"
         MOVE  DSP-AITSHO   TO  RUI-F23 *>相手商品CD
         MOVE  DSP-SOKCD    TO  RUI-F17 *>場所
         MOVE  ALL "9"      TO  RUI-F09 *>出荷日
     ELSE
         *> 初期表示で入力不可なので相手先商品ＣＤで移動
         MOVE  DSP-AITSHO   TO  RUI-F23 *>相手商品CD
         MOVE  HIGH-VALUE   TO  RUI-F17 *>場所
         MOVE  ALL "9"      TO  RUI-F09 *>出荷日
     END-IF.

     MOVE  "FWD"            TO  FG-FWD-BWD.
     MOVE  LOW-VALUE        TO  FG-RUISEKF-END.

     PERFORM  RUI-READ2-SEC.
     IF  FG-RUISEKF-END = "END"
         MOVE   9           TO  ERR-NO
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-AITSHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SOKCD
         GO TO  FWD-REC-EXIT
     END-IF.

     MOVE  RUI-F23          TO  DSP-AITSHO.
     IF  LINK-DSOKCD = "01"
         MOVE  RUI-F17      TO  DSP-SOKCD
     ELSE
         *> 初期表示で入力不可なのでそのまま
         CONTINUE
     END-IF.


     IF  DSP-SOKCD NOT = SPACE
         MOVE  DSP-SOKCD    TO  SOK-F01
         PERFORM  RD-ZSOKMS-SEC
         IF  FG-ZSOKMS-INV = 1
             MOVE  SPACE    TO  DSP-SOKNM
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKNM
         END-IF
     ELSE
         MOVE  SPACE        TO  DSP-SOKNM
     END-IF.

     MOVE  SPACE            TO  DSP-HIZUKE(1:).


 FWD-REC-EXIT.
     EXIT.
******************************************************************
*             ボディ処理
******************************************************************
 BODY-SEC               SECTION.
* ボディ部入力
     PERFORM  DSP-WR-SEC.
     PERFORM  DSP-RD-SEC.

     EVALUATE  DSP-FNC
       WHEN  "F005"
         MOVE  9            TO  END-FLG

       WHEN  "F004"
         PERFORM  DSP-INIT-SEC
         MOVE  "HEAD"       TO  SYORI-GROUP

       WHEN  "F006"
         MOVE  "HEAD"       TO  SYORI-GROUP

       WHEN  "E000"
         CONTINUE

       WHEN  "F011"
         PERFORM  FWD-PAGE-SEC

       WHEN  "F012"
         PERFORM  BWD-PAGE-SEC

       WHEN  OTHER
         MOVE  1          TO  ERR-NO

     END-EVALUATE.

 BODY-EXIT.
     EXIT.
******************************************************************
*             前頁処理
******************************************************************
 FWD-PAGE-SEC        SECTION.

     IF  CNT-PAGE = 1
         MOVE  6            TO  ERR-NO
         GO TO  FWD-PAGE-EXIT
     END-IF.

     COMPUTE  CNT-PAGE = CNT-PAGE - 1.
     PERFORM  BODY-SET-SEC.

 FWD-PAGE-EXIT.
     EXIT.
******************************************************************
*             次頁処理
******************************************************************
 BWD-PAGE-SEC          SECTION.

     IF  CNT-PAGE >= MAX-PAGE
         MOVE 7             TO  ERR-NO
         GO TO  BWD-PAGE-EXIT
     END-IF.

     COMPUTE  CNT-PAGE = CNT-PAGE + 1.
     PERFORM  BODY-SET-SEC.

 BWD-PAGE-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE  RUISEKL
            SHOTBL
            HJYOKEN
            ZSOKMS
            SGYKBMF
            TOKMS3
            DSPFILE.
 END-EXIT.
     EXIT.
