***************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　在庫管理システム
*    モジュール名　　　　：　在庫照会（量販店商品ＣＤ）
*    処理概要　　　　　　：　量販店商品ＣＤ在庫を一覧表示する
*    作成日／更新日　　　：　2011/10/11
*    作成者／更新者　　　：　飯田/NAV
*
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SZA0112I.
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
*在庫マスタ（商品コード・倉庫）
     SELECT      ZAMZAIF  ASSIGN    TO        DA-01-VI-ZAMZAIL2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY
                             ZAI-F02 *> 商品ＣＤＧ
                             ZAI-F01 *> 倉庫ＣＤ
                             ZAI-F03 *> _番
                           FILE      STATUS    ZAI-ST   ZAI-ST1.
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
*商品変換テーブル
     SELECT      SHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL14
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY
                             SHT-F02  *>相手先商品CD
                             SHT-F03  *>商品CDG
                           FILE      STATUS    SHT-ST    SHT-ST1.
*取引先マスタ
     SELECT      TOKMS    ASSIGN    TO        DA-01-VI-TOKMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F52
                           FILE      STATUS    TOK-ST    TOK-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*画面ファイル
 FD  DSPFILE.
     COPY        FZA01121  OF        XMDLIB
     JOINING     DSP       AS        PREFIX.
*在庫マスタ
 FD  ZAMZAIF.
     COPY        ZAMZAIF   OF        XFDLIB
     JOINING     ZAI      AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYOKEN    AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*商品変換ＴＢＬ
 FD  SHOTBL.
     COPY        SHOTBL7   OF        XFDLIB
     JOINING     SHT       AS        PREFIX.
*取引先マスタ
 FD  TOKMS.
     COPY        TOKMS3    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
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
     03  MSG01             PIC N(20) VALUE
     NC"無効ＰＦキーです".
     03  MSG02             PIC N(20) VALUE
     NC"　".
     03  MSG03             PIC N(20) VALUE
     NC"検索条件を入力して下さい".
     03  MSG04             PIC N(20) VALUE
     NC"　".
     03  MSG05             PIC N(20) VALUE
     NC"　".
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
     03  MSG12             PIC N(20) VALUE
     NC"商品変換テーブルに存在しません".
     03  MSG13             PIC N(20) VALUE
     NC"　".
     03  MSG14             PIC N(20) VALUE
     NC"　".
     03  MSG15             PIC N(20) VALUE
     NC"　".
     03  MSG16             PIC N(20) VALUE
     NC"　".
     03  MSG17             PIC N(20) VALUE
     NC"　".
     03  MSG18             PIC N(20) VALUE
     NC"　".
     03  MSG19             PIC N(20) VALUE
     NC"　".
     03  MSG20             PIC N(20) VALUE
     NC"　".
 01  MSG-AREAR             REDEFINES      MSG-AREA.
     03  MSGIX             PIC N(20)      OCCURS   20.
*
 01  PFKEY-MSG.
     03  PMSG01            PIC N(30) VALUE
       NC"_取消　_終了　_前レコード　_次レコード".
*---------------------------------------------------------------E
     03  PMSG02            PIC N(30) VALUE
       NC"_取消　_終了　_前頁　_次頁　_前レコード　_次レコ
-        "ード".
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                 PIC  9(08)  VALUE ZERO.
 01  DATE-AREA.
     03  WK-YS                   PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y                PIC  9(02)  VALUE ZERO.
         05  WK-M                PIC  9(02)  VALUE ZERO.
         05  WK-D                PIC  9(02)  VALUE ZERO.
 01  DATE-AREAR2  REDEFINES DATE-AREA.
     03  SYS-DATE                PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY           PIC  9(04)  VALUE ZERO.
     03  FILLER                  PIC  X(01)  VALUE "/".
     03  HEN-DATE-MM             PIC  9(02)  VALUE ZERO.
     03  FILLER                  PIC  X(01)  VALUE "/".
     03  HEN-DATE-DD             PIC  9(02)  VALUE ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH             PIC  9(02)  VALUE ZERO.
     03  FILLER                  PIC  X(01)  VALUE ":".
     03  HEN-TIME-MM             PIC  9(02)  VALUE ZERO.
     03  FILLER                  PIC  X(01)  VALUE ":".
     03  HEN-TIME-SS             PIC  9(02)  VALUE ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                  PIC  N(01)  VALUE NC"（".
     03  HEN-TOKHAN              PIC  N(06)  VALUE SPACE.
     03  FILLER                  PIC  N(01)  VALUE NC"）".

 01  WK-AREA.
     03  END-FLG                 PIC  9(01).
     03  SYORI-GROUP             PIC  X(04).
     03  ERR-NO                  PIC  9(02).

  *> 商品変換ＴＢＬの品単退避
     03  WK-SHT-HINTAN           PIC  X(08).

  *> 商品変換ＴＢＬレコード開始キー領域
     03  WK-SHOTBL-STKY.
       05  WK-SHOTBL-STKY-AITSHO    PIC  X(13).
       05  WK-SHOTBL-STKY-SHOCD-G.
         07  WK-SHOTBL-STKY-SHOCD   PIC  X(08).
         07  WK-SHOTBL-STKY-HINTAN  PIC  X(08).

  *> 在庫マスタ合計検索開始キー領域
     03  WK-TOTST-KEY.
       05  WK-TOTST-KEY-SHOCD-G.
         07  WK-TOTST-KEY-SHOCD   PIC  X(08).
         07  WK-TOTST-KEY-HINTAN  PIC  X(08).
       05  WK-TOTST-KEY-SOKCD     PIC  X(02).
       05  WK-TOTST-KEY-TANANO    PIC  X(06).

  *> 在庫マスタ頁開始キー領域
     03  WK-PGST-KEY.
       05  WK-PGST-KEY-SHOCD-G.
         07  WK-PGST-KEY-SHOCD   PIC  X(08).
         07  WK-PGST-KEY-HINTAN  PIC  X(08).
       05  WK-PGST-KEY-SOKCD     PIC  X(02).
       05  WK-PGST-KEY-TANANO    PIC  X(06).

  *> 在庫マスタ頁終了キー領域
     03  WK-PGED-KEY.
       05  WK-PGED-KEY-SHOCD-G.
         07  WK-PGED-KEY-SHOCD   PIC  X(08).
         07  WK-PGED-KEY-HINTAN  PIC  X(08).
       05  WK-PGED-KEY-SOKCD     PIC  X(02).
       05  WK-PGED-KEY-TANANO    PIC  X(06).

     03  FG-ZAMZAIF-END          PIC  X(03).
     03  FG-ZAMZAIF-START        PIC  X(01).
     03  FG-SHOTBL-END           PIC  X(03).
     03  FG-SHOTBL-START         PIC  X(01).
     03  FG-ZAMZAIF-RVS-END      PIC  X(03).
     03  FG-ZAMZAIF-RVS-START    PIC  X(01).
     03  FG-SHOTBL-RVS-END       PIC  X(03).
     03  FG-SHOTBL-RVS-START     PIC  X(01).
     03  FG-ZSOKMS-INV           PIC  9(01).
     03  FG-HMEIMS-INV           PIC  9(01).
     03  FG-RCD-PAG              PIC  X(02).
     03  FG-RCD-PAG-RVS          PIC  X(02).
     03  IX-GYO                  PIC  9(02).
     03  IX-GYO2                 PIC  9(02).
     03  IX-GYO-ST               PIC  9(02).
     03  IX-GYO-ST2              PIC  9(02).
     03  WK-GYO-SA               PIC  9(02).
     03  WK-KETA                 PIC  9(02).
     03  SV-DSP-RYSHCD           PIC  X(13).
     03  SV-DSP-SHOCD            PIC  X(08).
     03  SV-DSP-KANANM           PIC  X(15).
     03  SV-DSP-SHONM            PIC  N(30).

 01  WK-SHOCD.
     03  WK-SHO            PIC  X(01)  OCCURS 8.
 01  IX-SHO                PIC  9(02).

 01  WK-TANA.
     03  WK-TANA1          PIC  X(01).
     03  FILLER            PIC  X(01)    VALUE "-".
     03  WK-TANA2          PIC  X(03).
     03  FILLER            PIC  X(01)    VALUE "-".
     03  WK-TANA3          PIC  X(02).

 01  TOTAL-AREA.
     03  TTL-F04           PIC S9(11)V99.
     03  TTL-F05           PIC S9(11)V99.
     03  TTL-F07           PIC S9(11)V99.
     03  TTL-F15           PIC S9(11)V99.
     03  TTL-F08           PIC S9(11)V99.
     03  TTL-F16           PIC S9(11)V99.
     03  TTL-F26           PIC S9(11)V99.
     03  TTL-F27           PIC S9(11)V99.
     03  TTL-F28           PIC S9(11)V99.
     03  TTL-HIKIAK        PIC S9(11)V99.

 01  WK-HIKIAK             PIC S9(11)V99.

 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  ZAI-ST            PIC X(02).
     03  ZAI-ST1           PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  JYKN-ST           PIC X(02).
     03  JYKN-ST1          PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
     03  SHT-ST            PIC X(02).
     03  SHT-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).

     03  FILE-ERR1         PIC N(20) VALUE
     NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(20) VALUE
     NC"商品在庫マスタ異常！".
     03  FILE-ERR4         PIC N(20) VALUE
     NC"商品名称マスタ異常！".
     03  FILE-ERR5         PIC N(10) VALUE
     NC"条件ファイル異常！".
     03  FILE-ERR6         PIC N(10) VALUE
     NC"倉庫マスタ異常！".
     03  FILE-ERR7         PIC N(10) VALUE
     NC"商品変換ＴＢＬ異常！".
     03  FILE-ERR8         PIC N(10) VALUE
     NC"取引先マスタ異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 LINKAGE                SECTION.
******************************************************************
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
******************************************************************
 PROCEDURE              DIVISION       USING     LINK-SOKCD
                                                 LINK-DSOKCD.
******************************************************************
*             ファイルエラー処理
******************************************************************
 DECLARATIVES.
 DSP-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         DSPFILE.
     DISPLAY       DSP-ST    UPON      STA.
     DISPLAY       DSP-ST1   UPON      STA.
     DISPLAY       FILE-ERR1 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 ZAI-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         ZAMZAIF.
     DISPLAY       ZAI-ST    UPON      STA.
     DISPLAY       ZAI-ST1   UPON      STA.
     DISPLAY       FILE-ERR2 UPON      STA.
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
 SHOTBL-ERR             SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         SHOTBL.
     DISPLAY       SHT-ST    UPON      STA.
     DISPLAY       SHT-ST1   UPON      STA.
     DISPLAY       FILE-ERR7 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 TOK-ERR                SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         TOKMS.
     DISPLAY       TOK-ST    UPON      STA.
     DISPLAY       TOK-ST1   UPON      STA.
     DISPLAY       FILE-ERR8 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 END DECLARATIVES.
******************************************************************
*             メイン
******************************************************************
 SHORI-SEC              SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = 9.
     PERFORM  END-SEC.
     STOP RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*             初期処理
******************************************************************
 INIT-SEC               SECTION.
     OPEN  INPUT ZAMZAIF.
     OPEN  INPUT HMEIMS.
     OPEN  INPUT HJYOKEN.
     OPEN  INPUT ZSOKMS.
     OPEN  INPUT SHOTBL.
     OPEN  INPUT TOKMS.
     OPEN  I-O   DSPFILE.

     MOVE  SPACE            TO  WK-AREA.
     INITIALIZE  WK-AREA.

     MOVE  "HEAD"           TO  SYORI-GROUP.
     PERFORM  DSP-INIT-SEC.
 INIT-EXIT.
     EXIT.
******************************************************************
*             初期画面　表示
******************************************************************
 DSP-INIT-SEC           SECTION.

     IF  SYORI-GROUP = "HEAD"
         MOVE  SPACE        TO  DSP-FZA01121
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SOKCD
         MOVE  "M"          TO  EDIT-OPTION OF DSP-SOKCD
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-RYSHCD
         MOVE  "M"          TO  EDIT-OPTION OF DSP-RYSHCD
     ELSE
         MOVE  SPACE        TO  DSP-BODYG
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KAKU
         MOVE  "M"          TO  EDIT-OPTION OF DSP-KAKU
     END-IF.

* 特販部名称編集
     MOVE  SPACE            TO  JYOKEN-REC.
     INITIALIZE  JYOKEN-REC.
     MOVE  "99"             TO  JYOKEN-F01.
     MOVE  "BUMON"          TO  JYOKEN-F02.
     READ  HJYOKEN
       INVALID KEY
         MOVE  NC"＊＊＊＊＊＊"  TO  HEN-TOKHAN
       NOT INVALID KEY
         MOVE  JYOKEN-F03        TO  HEN-TOKHAN
     END-READ.

     MOVE  HEN-TOKHAN-AREA  TO  DSP-TOKHAN.

     IF  LINK-DSOKCD = "01" OR "88"
         CONTINUE
     ELSE
         MOVE LINK-SOKCD    TO  DSP-SOKCD
         MOVE LINK-SOKCD    TO  SOK-F01
         PERFORM  RD-ZSOKMS-SEC
         IF  FG-ZSOKMS-INV = 1
             CONTINUE
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKNM
         END-IF
      *> プロテクトをかける
         MOVE  "X"  TO EDIT-STATUS OF DSP-SOKCD

     END-IF.

 DSP-INIT-EXIT.
     EXIT.
******************************************************************
*  倉庫マスタ検索
******************************************************************
 RD-ZSOKMS-SEC         SECTION.
     READ  ZSOKMS
       INVALID
         MOVE  1            TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZSOKMS-INV
     END-READ.

 RD-ZSOKMS-EXIT.
     EXIT.
******************************************************************
*             主処理
******************************************************************
 MAIN-SEC               SECTION.

     EVALUATE  SYORI-GROUP
       WHEN  "HEAD"
         PERFORM  HEAD-SEC
       WHEN  "BODY"
         PERFORM  BODY-SEC
       WHEN  OTHER
         DISPLAY "SYORI-GROUP ERROR ]]] SYORI-GROUP="
                 SYORI-GROUP "*"  UPON CONS
         MOVE  4000         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
******************************************************************
*             ヘッド処理
******************************************************************
 HEAD-SEC               SECTION.
* ヘッド部入力

     PERFORM  DSP-WT-SEC.

     EVALUATE  DSP-FNC
       WHEN  "E000" *> 処理
         PERFORM  HD-CHK-SEC
         IF  ERR-NO = ZERO
             MOVE "BODY"    TO SYORI-GROUP
         END-IF

       WHEN  "F004" *> 取消
         PERFORM  DSP-INIT-SEC

       WHEN  "F005" *> 終了
         MOVE  9            TO  END-FLG
*********MOVE  4010         TO  PROGRAM-STATUS

       WHEN  "F011" *> 前レコード
         PERFORM  BWD-REC-SEC
         IF  ERR-NO = ZERO
             MOVE "BODY"    TO SYORI-GROUP
         END-IF

       WHEN  "F012" *> 次レコード
         PERFORM  FWD-REC-SEC
         IF  ERR-NO = ZERO
             MOVE "BODY"    TO SYORI-GROUP
         END-IF

       WHEN  OTHER
         MOVE  1            TO  ERR-NO
     END-EVALUATE.

 HEAD-EXIT.
     EXIT.
****************************************************************
*             画面    ＷＲＩＴＥ
****************************************************************
 DSP-WT-SEC             SECTION.
* システム日、時刻編集
     PERFORM  SYSDT-SEC.
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.

* メッセージ編集
     IF  ERR-NO = ZERO
         MOVE  SPACE           TO  DSP-MO001
     ELSE
         MOVE  MSGIX (ERR-NO)  TO  DSP-MO001
     END-IF.

* ＰＦキーガイド編集
     IF  SYORI-GROUP = "HEAD"
         MOVE  PMSG01       TO  DSP-MO002
     ELSE
         MOVE  PMSG02       TO  DSP-MO002
     END-IF.

     MOVE  "FZA01121"       TO  DSP-FMT.
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  SPACE            TO  DSP-PRO.
* 画面書込み
     WRITE  DSP-FZA01121.

     EVALUATE  SYORI-GROUP
       WHEN  "HEAD"
         MOVE  "HEAD"       TO  DSP-GRP
       WHEN  "BODY"
         MOVE  "BODY"       TO  DSP-GRP
     END-EVALUATE.

     MOVE  "NE"             TO  DSP-PRO.
* 画面読込み
     READ  DSPFILE.

     MOVE  ZERO             TO  ERR-NO.

 DSP-WT-EXIT.
     EXIT.
******************************************************************
*   システム日時刻取得
******************************************************************
 SYSDT-SEC               SECTION.
* システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
* 画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
* システム日付取得
     ACCEPT  WK-TIME   FROM TIME.
* 画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.

 SYSDT-EXIT.
     EXIT.
****************************************************************
*  ヘッダチェック処理
****************************************************************
 HD-CHK-SEC             SECTION.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SOKCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-RYSHCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-RYSHCD.
* 倉庫コード
     MOVE  SPACE            TO  DSP-SOKNM.

     IF  DSP-SOKCD = SPACE
         GO TO  HD-CHK-SOKCD-END
     END-IF.

     MOVE  DSP-SOKCD        TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC
     IF  FG-ZSOKMS-INV = 1
         IF  ERR-NO = ZERO
             MOVE  10       TO  ERR-NO
         END-IF
         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
     ELSE
         MOVE  SOK-F02      TO  DSP-SOKNM
     END-IF.

 HD-CHK-SOKCD-END.

* 量販店商品コード
     IF  DSP-FNC = "F011" OR "F012"
         *> 前レコード、次レコード時は相手先商品ＣＤと
         *> 商品ＣＤを商品変換テーブルより画面に転送し
         *> ているためこの処理は不要
         GO TO  HD-CHK-AITSHO-END
     END-IF.

     IF  DSP-RYSHCD = SPACE
         IF  ERR-NO =  ZERO
             MOVE  3        TO  ERR-NO
         END-IF
         MOVE  "R"  TO  EDIT-OPTION OF DSP-RYSHCD
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-RYSHCD
         GO TO  HD-CHK-AITSHO-END
     END-IF.

     *> 商品変換ＴＢＬのチェック
     INITIALIZE  SHT-REC.
     MOVE  DSP-RYSHCD       TO  SHT-F02. *> 相手先商品ＣＤ
     MOVE  LOW-VALUE        TO  FG-SHOTBL-END.
     MOVE  SPACE            TO  FG-SHOTBL-START.
     PERFORM  RD-SHOTBL-FWD-SEC.
     IF  FG-SHOTBL-END = "END"
         IF  ERR-NO =  ZERO
             MOVE  12       TO  ERR-NO
         END-IF
         MOVE  "R"  TO  EDIT-OPTION OF DSP-RYSHCD
         MOVE  "C"  TO  EDIT-CURSOR OF DSP-RYSHCD
         GO TO  HD-CHK-AITSHO-END
     END-IF.

  *> 商品変換ＴＢＬレコード開始キー領域
     MOVE  SHT-F02          TO  WK-SHOTBL-STKY-AITSHO.
     MOVE  SHT-F031         TO  WK-SHOTBL-STKY-SHOCD.
     MOVE  SHT-F032         TO  WK-SHOTBL-STKY-HINTAN.

  *> 商品ＣＤ
     MOVE  SHT-F031         TO  DSP-SHOCD. *> 商品ＣＤ
     MOVE  DSP-SHOCD        TO  WK-SHOCD.
     PERFORM  SHOCD-CV-SEC.
     MOVE  WK-SHOCD         TO  DSP-SHOCD.

  *> 検索品単の設定
     MOVE  SHT-F032         TO  WK-SHT-HINTAN.

 HD-CHK-AITSHO-END.
     IF  ERR-NO NOT =  ZERO
         GO TO  HD-CHK-EXIT
     END-IF.

* データ存在チェック
     MOVE  SPACE            TO  DSP-SHONM.
     MOVE  SPACE            TO  DSP-KANANM.
     PERFORM  DATA-CHK-SEC.
     IF  ERR-NO NOT =  ZERO
         GO TO  HD-CHK-EXIT
     END-IF.

* 商品名
     MOVE  ZAI-F021         TO  MEI-F011. *> 商品ＣＤ
     MOVE  ZAI-F022         TO  MEI-F012. *> 品単
     PERFORM  RD-HMEIMS-SEC.
     IF  FG-HMEIMS-INV = 1
         MOVE  ALL NC"＊"   TO  DSP-SHONM
     ELSE
         MOVE  MEI-F02      TO  DSP-SHONM
     END-IF.

     MOVE  ZAI-F30          TO  DSP-KANANM.

     MOVE "RC"              TO  FG-RCD-PAG.
     PERFORM  GMN-MSEDT-SEC.

 HD-CHK-EXIT.
     EXIT.
******************************************************************
*    商品検索テーブル読込み 処理 (昇順）
******************************************************************
 RD-SHOTBL-FWD-SEC     SECTION.

     IF  FG-SHOTBL-END = LOW-VALUE
         MOVE  SPACE        TO  FG-SHOTBL-END

         IF  FG-SHOTBL-START = SPACE
             START  SHOTBL  KEY >= SHT-F02 SHT-F03
               INVALID  KEY
                 MOVE  "END"    TO  FG-SHOTBL-END
                 GO TO  RD-SHOTBL-FWD-090
             END-START
         ELSE
             START  SHOTBL  KEY > SHT-F02 SHT-F03
               INVALID  KEY
                 MOVE  "END"    TO  FG-SHOTBL-END
                 GO TO  RD-SHOTBL-FWD-090
             END-START
         END-IF

     END-IF.

     READ  SHOTBL
       AT END
         MOVE  "END"        TO  FG-SHOTBL-END
         GO TO  RD-SHOTBL-FWD-090
     END-READ.

     IF  FG-SHOTBL-START = SPACE
         IF  SHT-F02 > DSP-RYSHCD *> 相手先商品ＣＤ
             MOVE  "END"    TO  FG-SHOTBL-END
         END-IF
     END-IF.

 RD-SHOTBL-FWD-090.
     IF  FG-SHOTBL-START NOT = SPACE
         *> 開始フラグを標準にする。
         MOVE  SPACE        TO  FG-SHOTBL-START
     END-IF.

 RD-SHOTBL-FWD-EXIT.
     EXIT.
******************************************************************
*    商品コード変換
******************************************************************
 SHOCD-CV-SEC               SECTION.
     IF  WK-SHOCD = SPACE
         GO TO  SHOCD-CV-EXIT
     END-IF.

     PERFORM  UNTIL  WK-SHO(8) NOT = SPACE

       PERFORM  VARYING IX-SHO  FROM 7 BY -1
                UNTIL   IX-SHO = 0
         MOVE  WK-SHO(IX-SHO)   TO  WK-SHO(IX-SHO + 1)

       END-PERFORM

       MOVE  ZERO           TO  WK-SHO(1)

     END-PERFORM.

 SHOCD-CV-EXIT.
     EXIT.
******************************************************************
*  商品名称マスタ検索
******************************************************************
 RD-HMEIMS-SEC         SECTION.
     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.
 RD-HMEIMS-EXIT.
     EXIT.
******************************************************************
*    データチェック処理
******************************************************************
 DATA-CHK-SEC          SECTION.

     INITIALIZE  ZAI-REC.
     MOVE  SHT-F03          TO  ZAI-F02. *> 商品ＣＤＧ
     MOVE  DSP-SOKCD        TO  ZAI-F01. *> 倉庫ＣＤ

  *> 在庫マスタ合計検索開始キー設定
     MOVE  ZAI-F021         TO  WK-TOTST-KEY-SHOCD.
     MOVE  ZAI-F022         TO  WK-TOTST-KEY-HINTAN.
     MOVE  ZAI-F01          TO  WK-TOTST-KEY-SOKCD.
     MOVE  ZAI-F03          TO  WK-TOTST-KEY-TANANO.

     MOVE  LOW-VALUE            TO  FG-ZAMZAIF-END.
     PERFORM  RD-ZAMZAIF-FWD-SEC.
     IF  FG-ZAMZAIF-END = "END"
         IF  ERR-NO = ZERO
             MOVE  11       TO  ERR-NO
         END-IF
         GO TO  DATA-CHK-EXIT
     END-IF.

 DATA-CHK-EXIT.
     EXIT.
******************************************************************
*    商品在庫マスタ読込み（昇順）
******************************************************************
 RD-ZAMZAIF-FWD-SEC    SECTION.

     IF  FG-ZAMZAIF-END = LOW-VALUE
         MOVE  SPACE        TO  FG-ZAMZAIF-END
         IF  FG-ZAMZAIF-START = SPACE
             START  ZAMZAIF  KEY >= ZAI-F02
                                    ZAI-F01
                                    ZAI-F03
               INVALID KEY
                 MOVE  "END"   TO  FG-ZAMZAIF-END
                 GO TO  RD-ZAMZAIF-FWD-EXIT
             END-START
         ELSE
             MOVE  SPACE    TO  FG-ZAMZAIF-START
             START  ZAMZAIF  KEY > ZAI-F02
                                   ZAI-F01
                                   ZAI-F03
               INVALID KEY
                 MOVE  "END"   TO  FG-ZAMZAIF-END
                 GO TO  RD-ZAMZAIF-FWD-EXIT
             END-START
     END-IF.

     READ  ZAMZAIF  NEXT
       AT END
         MOVE  "END"        TO  FG-ZAMZAIF-END
         GO TO  RD-ZAMZAIF-FWD-EXIT
     END-READ.

     IF  ZAI-F021 > DSP-SHOCD *> 商品ＣＤ
         MOVE  "END"        TO  FG-ZAMZAIF-END
         GO TO  RD-ZAMZAIF-FWD-EXIT
     END-IF.

     IF       ZAI-F021 = DSP-SHOCD
         AND  ZAI-F022 > WK-SHT-HINTAN *> 品単
         MOVE  "END"        TO  FG-ZAMZAIF-END
         GO TO  RD-ZAMZAIF-FWD-EXIT
     END-IF.

     IF  DSP-SOKCD = SPACE
         CONTINUE
     ELSE
         IF  ZAI-F01 > DSP-SOKCD *> 倉庫ＣＤ
             GO TO  RD-ZAMZAIF-FWD-SEC
         END-IF
     END-IF.

 RD-ZAMZAIF-FWD-EXIT.
     EXIT.
******************************************************************
*    画面編集
******************************************************************
 GMN-MSEDT-SEC               SECTION.
     MOVE  ZERO             TO  IX-GYO.
     IF  DSP-SOKCD = SPACE
         *> 合計行作成
         PERFORM  GMN-MSEDT-TOT-SEC
     END-IF.

     PERFORM  GMN-MSEDTB-SEC.

 GMN-MSEDT-EXIT.
     EXIT.
******************************************************************
*    画面編集（合計行）
******************************************************************
 GMN-MSEDT-TOT-SEC               SECTION.
     MOVE  ZERO             TO  TOTAL-AREA.

     MOVE  WK-TOTST-KEY-SHOCD     TO  ZAI-F021.
     MOVE  WK-TOTST-KEY-HINTAN    TO  ZAI-F022.
     MOVE  WK-TOTST-KEY-SOKCD     TO  ZAI-F01.
     MOVE  WK-TOTST-KEY-TANANO    TO  ZAI-F03.
     MOVE  LOW-VALUE             TO  FG-ZAMZAIF-END
     PERFORM  RD-ZAMZAIF-FWD-SEC.
     IF  FG-ZAMZAIF-END = "END"
         GO TO  GMN-MSEDT-TOT-EXIT
     END-IF.

     PERFORM  UNTIL FG-ZAMZAIF-END = "END"

       PERFORM  GMN-MSEDT-TOTB-SEC *> 集計
       PERFORM  RD-ZAMZAIF-FWD-SEC

     END-PERFORM.

     PERFORM  GMN-MSEDT-TOTB2-SEC. *> 編集

 GMN-MSEDT-TOT-EXIT.
     EXIT.
******************************************************************
*    画面編集（合計行）Ｂ
******************************************************************
 GMN-MSEDT-TOTB-SEC    SECTION.
     ADD  ZAI-F04   TO  TTL-F04.
     ADD  ZAI-F05   TO  TTL-F05.
     ADD  ZAI-F07   TO  TTL-F07.
     ADD  ZAI-F08   TO  TTL-F08.
     ADD  ZAI-F26   TO  TTL-F26.
     ADD  ZAI-F27   TO  TTL-F27.
     ADD  ZAI-F28   TO  TTL-F28.
     COMPUTE  WK-HIKIAK = ZAI-F04 - ZAI-F27.
     ADD  WK-HIKIAK TO  TTL-HIKIAK.

 GMN-MSEDT-TOTB-EXIT.
     EXIT.
******************************************************************
*    画面編集（合計行）Ｂ２
******************************************************************
 GMN-MSEDT-TOTB2-SEC   SECTION.
     ADD  1   TO  IX-GYO.
     MOVE  NC"合計"         TO  DSP-SOKO   (IX-GYO).
     MOVE  SPACE            TO  DSP-HTAN1  (IX-GYO).
     MOVE  SPACE            TO  DSP-HTAN2  (IX-GYO).
     MOVE  SPACE            TO  DSP-HTAN3  (IX-GYO).
     MOVE  SPACE            TO  DSP-TANA   (IX-GYO)

     MOVE  TTL-F04          TO  DSP-GEN    (IX-GYO).
     MOVE  TTL-F05          TO  DSP-ZEN    (IX-GYO).
     MOVE  TTL-F07          TO  DSP-TONYK  (IX-GYO).
     MOVE  TTL-F08          TO  DSP-TOSYK  (IX-GYO).
     MOVE  TTL-F26          TO  DSP-HACZAN (IX-GYO).
     MOVE  TTL-F27          TO  DSP-MISYUK (IX-GYO).
     MOVE  TTL-F28          TO  DSP-HIKIAZ (IX-GYO).
     MOVE  TTL-HIKIAK       TO  DSP-HIKIAK (IX-GYO).

 GMN-MSEDT-TOTB2-EXIT.
     EXIT.
******************************************************************
*    画面編集Ｂ
******************************************************************
 GMN-MSEDTB-SEC        SECTION.

     IF  DSP-SOKCD = SPACE *> 合計集計の場合再度読込む
         IF  FG-RCD-PAG = "RC" *> 次レコード、前レコード処理
             INITIALIZE  ZAI-REC
             MOVE  SHT-F03       TO  ZAI-F02 *> 商品ＣＤＧ
             MOVE  DSP-SOKCD     TO  ZAI-F01 *> 倉庫ＣＤ

             MOVE  LOW-VALUE     TO  FG-ZAMZAIF-END
         ELSE                  *> 次頁処理
             INITIALIZE  ZAI-REC
             MOVE  WK-PGED-KEY-SHOCD-G TO  ZAI-F02 *> 商品ＣＤ
             MOVE  WK-PGED-KEY-SOKCD   TO  ZAI-F01 *> 倉庫ＣＤ
             MOVE  WK-PGED-KEY-TANANO  TO  ZAI-F03 *> _番

             MOVE  LOW-VALUE     TO  FG-ZAMZAIF-END
             MOVE  ">"           TO  FG-ZAMZAIF-START
         END-IF

         PERFORM  RD-ZAMZAIF-FWD-SEC
         IF  FG-ZAMZAIF-END = "END"
             GO TO  GMN-MSEDTB-EXIT
         END-IF

     END-IF.

  *> 頁制御キーの初期化
     INITIALIZE  WK-PGST-KEY.
     INITIALIZE  WK-PGED-KEY.

* データ行の編集
     PERFORM  UNTIL   IX-GYO >= 7
                   OR FG-ZAMZAIF-END = "END"
       COMPUTE  IX-GYO = IX-GYO + 1
       PERFORM  GMN-MSEDT-PGKYSV-SEC
       PERFORM  GMN-MSEDTC-SEC
       PERFORM  RD-ZAMZAIF-FWD-SEC

     END-PERFORM.

* 空白行の編集
     IF  IX-GYO < 7
         ADD  1   TO  IX-GYO
         PERFORM  VARYING IX-GYO  FROM IX-GYO BY 1
                  UNTIL   IX-GYO > 7
            MOVE  SPACE     TO  DSP-BODY01 (IX-GYO)

         END-PERFORM
     END-IF.

 GMN-MSEDTB-EXIT.
     EXIT.
******************************************************************
*    画面頁制御キー退避
******************************************************************
 GMN-MSEDT-PGKYSV-SEC  SECTION.
* 頁開始キーの退避
     IF  DSP-SOKCD = SPACE *> 合計行あり
         IF  IX-GYO = 2
             MOVE  ZAI-F02  TO  WK-PGST-KEY-SHOCD-G *>商品ＣＤ
             MOVE  ZAI-F01  TO  WK-PGST-KEY-SOKCD   *>倉庫ＣＤ
             MOVE  ZAI-F03  TO  WK-PGST-KEY-TANANO  *>_番
         END-IF
     ELSE                  *> 通常の場合
         IF  IX-GYO = 1
             MOVE  ZAI-F02  TO  WK-PGST-KEY-SHOCD-G *>商品ＣＤ
             MOVE  ZAI-F01  TO  WK-PGST-KEY-SOKCD   *>倉庫ＣＤ
             MOVE  ZAI-F03  TO  WK-PGST-KEY-TANANO  *>_番
         END-IF
     END-IF.

* 頁終了キーの退避
     MOVE  ZAI-F02       TO  WK-PGED-KEY-SHOCD-G.  *> 商品ＣＤ
     MOVE  ZAI-F01       TO  WK-PGED-KEY-SOKCD.    *> 倉庫ＣＤ
     MOVE  ZAI-F03       TO  WK-PGED-KEY-TANANO.   *> _番

 GMN-MSEDT-PGKYSV-EXIT.
     EXIT.
******************************************************************
*    画面編集Ｃ
******************************************************************
 GMN-MSEDTC-SEC        SECTION.

* 倉庫名
     IF  DSP-SOKCD = SPACE
         MOVE  ZAI-F01      TO  SOK-F01
         PERFORM  RD-ZSOKMS-SEC
         IF  FG-ZSOKMS-INV = 1
             MOVE  SPACE    TO  DSP-SOKO (IX-GYO)
         ELSE
             MOVE  SOK-F02  TO  DSP-SOKO (IX-GYO)
         END-IF
     ELSE
         MOVE  DSP-SOKNM    TO  DSP-SOKO (IX-GYO)
     END-IF.

* 品単
     MOVE  ZAI-F022 (1:5)   TO  DSP-HTAN1 (IX-GYO).
     MOVE  ZAI-F022 (6:2)   TO  DSP-HTAN2 (IX-GYO).
     MOVE  ZAI-F022 (8:1)   TO  DSP-HTAN3 (IX-GYO).
* _番
     IF  ZAI-F03 NOT = SPACE
         MOVE  ZAI-F03(1:1) TO  WK-TANA1
         MOVE  ZAI-F03(2:3) TO  WK-TANA2
         MOVE  ZAI-F03(5:2) TO  WK-TANA3
         MOVE  WK-TANA      TO  DSP-TANA (IX-GYO)
     ELSE
         MOVE  SPACE        TO  DSP-TANA (IX-GYO)
     END-IF.

     MOVE  ZAI-F04   TO  DSP-GEN    (IX-GYO). *> 現在庫数
     MOVE  ZAI-F05   TO  DSP-ZEN    (IX-GYO). *> 前月未在庫数
     MOVE  ZAI-F07   TO  DSP-TONYK  (IX-GYO). *> 当月入庫数
     MOVE  ZAI-F08   TO  DSP-TOSYK  (IX-GYO). *> 当月出庫数
     MOVE  ZAI-F26   TO  DSP-HACZAN (IX-GYO). *> 未入庫数
     MOVE  ZAI-F27   TO  DSP-MISYUK (IX-GYO). *> 未出庫数
     MOVE  ZAI-F28   TO  DSP-HIKIAZ (IX-GYO). *> 引当済数

* 引当可能数
     COMPUTE  WK-HIKIAK =
         DSP-GEN (IX-GYO) - DSP-MISYUK (IX-GYO).
     MOVE  WK-HIKIAK        TO  DSP-HIKIAK (IX-GYO).

 GMN-MSEDTC-EXIT.
     EXIT.
******************************************************************
*  前レコード検索処理
******************************************************************
 BWD-REC-SEC           SECTION.
* 前レコード検索
     MOVE  LOW-VALUE        TO  FG-SHOTBL-RVS-END.
 BWD-REC-010.

     IF  DSP-RYSHCD NOT = WK-SHOTBL-STKY-AITSHO
         INITIALIZE  SHT-REC
         MOVE  DSP-RYSHCD       TO  SHT-F02  *>相手先商品CD
     ELSE   *> 前回キーを設定
         MOVE  WK-SHOTBL-STKY-AITSHO  TO  SHT-F02
         MOVE  WK-SHOTBL-STKY-SHOCD   TO  SHT-F031
         MOVE  WK-SHOTBL-STKY-HINTAN  TO  SHT-F032
     END-IF.

     MOVE  "<"              TO  FG-SHOTBL-RVS-START.
* 商品変換テーブル検索
     PERFORM  RD-SHOTBL-BWD-SEC.
     IF  FG-SHOTBL-RVS-END = "END"
         IF  ERR-NO = ZERO
             MOVE  8        TO  ERR-NO
         END-IF

         *> 退避したヘッド情報より復元
         MOVE  SV-DSP-RYSHCD  TO  DSP-RYSHCD
         MOVE  SV-DSP-SHOCD   TO  DSP-SHOCD
         MOVE  SV-DSP-KANANM  TO  DSP-KANANM
         MOVE  SV-DSP-SHONM   TO  DSP-SHONM
         GO TO  BWD-REC-EXIT
     END-IF.

  *> 商品変換ＴＢＬレコード開始キー領域
     MOVE  SHT-F02          TO  WK-SHOTBL-STKY-AITSHO.
     MOVE  SHT-F031         TO  WK-SHOTBL-STKY-SHOCD.
     MOVE  SHT-F032         TO  WK-SHOTBL-STKY-HINTAN.

* 前レコード相手先商品ＣＤの画面更新
     MOVE  SHT-F02          TO  DSP-RYSHCD.  *>相手先商品ＣＤ
     MOVE  SHT-F031         TO  DSP-SHOCD.   *>商品ＣＤ
     MOVE  DSP-SHOCD        TO  WK-SHOCD.
     PERFORM  SHOCD-CV-SEC.
     MOVE  WK-SHOCD         TO  DSP-SHOCD.
  *> 検索品単の設定
     MOVE  SHT-F032         TO  WK-SHT-HINTAN.

* 前レコード商品ＣＤＧで在庫マスタを順検索
     MOVE  SPACE            TO  DSP-SHONM.
     MOVE  SPACE            TO  DSP-KANANM.

     IF  SYORI-GROUP = "HEAD" *> ヘッド部の場合
         PERFORM  HD-CHK-SEC
         IF  ERR-NO NOT = ZERO
             GO TO  BWD-REC-EXIT
         END-IF
     ELSE                     *> 明細部の場合
         INITIALIZE  ZAI-REC
         MOVE  SHT-F03      TO  ZAI-F02 *> 商品ＣＤＧ
         MOVE  DSP-SOKCD    TO  ZAI-F01 *> 倉庫ＣＤ
         MOVE  LOW-VALUE    TO  ZAI-F03 *> _番

         *> 在庫マスタ合計検索開始キー設定
         MOVE  ZAI-F021     TO  WK-TOTST-KEY-SHOCD
         MOVE  ZAI-F022     TO  WK-TOTST-KEY-HINTAN
         MOVE  ZAI-F01      TO  WK-TOTST-KEY-SOKCD
         MOVE  ZAI-F03      TO  WK-TOTST-KEY-TANANO

         MOVE  LOW-VALUE         TO  FG-ZAMZAIF-END

      *> 在庫マスタ検索
         PERFORM  RD-ZAMZAIF-FWD-SEC
         IF  FG-ZAMZAIF-END = "END"
             GO TO  BWD-REC-010
         END-IF

     END-IF.

     MOVE  ZAI-F021         TO  MEI-F011. *> 商品ＣＤ
     MOVE  ZAI-F022         TO  MEI-F012. *> 品単
     PERFORM  RD-HMEIMS-SEC.
     IF  FG-HMEIMS-INV = 1
         MOVE  ALL NC"＊"   TO  DSP-SHONM
     ELSE
         MOVE  MEI-F02      TO  DSP-SHONM
     END-IF.

     MOVE  ZAI-F30          TO  DSP-KANANM.

   *> レコードなし時のために退避する。
     MOVE  DSP-RYSHCD       TO  SV-DSP-RYSHCD.
     MOVE  DSP-SHOCD        TO  SV-DSP-SHOCD.
     MOVE  DSP-KANANM       TO  SV-DSP-KANANM.
     MOVE  DSP-SHONM        TO  SV-DSP-SHONM.

* 前レコードの画面編集
     MOVE "RC"              TO  FG-RCD-PAG-RVS.
     PERFORM  GMN-MSEDT-SEC.
 BWD-REC-EXIT.
     EXIT.
******************************************************************
*    商品検索テーブル読込み 処理 (降順）
******************************************************************
 RD-SHOTBL-BWD-SEC     SECTION.

     IF  FG-SHOTBL-RVS-END = LOW-VALUE
         MOVE  SPACE        TO  FG-SHOTBL-RVS-END

         IF  FG-SHOTBL-RVS-START = SPACE
             START  SHOTBL  KEY <= SHT-F02 SHT-F03
                      WITH  REVERSED ORDER
               INVALID  KEY
                 MOVE  "END"    TO  FG-SHOTBL-RVS-END
                 GO TO  RD-SHOTBL-BWD-090
             END-START
         ELSE
             START  SHOTBL  KEY < SHT-F02 SHT-F03
                      WITH  REVERSED ORDER
               INVALID  KEY
                 MOVE  "END"    TO  FG-SHOTBL-RVS-END
                 GO TO  RD-SHOTBL-BWD-090
             END-START
         END-IF

     END-IF.

     READ  SHOTBL
       AT END
         MOVE  "END"        TO  FG-SHOTBL-RVS-END
         GO TO  RD-SHOTBL-BWD-090
     END-READ.

     IF  FG-SHOTBL-RVS-START = SPACE
         IF  SHT-F02 < DSP-RYSHCD *> 相手先商品ＣＤ
             MOVE  "END"    TO  FG-SHOTBL-RVS-END
         END-IF
     END-IF.

 RD-SHOTBL-BWD-090.
     IF  FG-SHOTBL-RVS-START NOT = SPACE
         *> 開始フラグを標準にする。
         MOVE  SPACE        TO  FG-SHOTBL-RVS-START
     END-IF.

 RD-SHOTBL-BWD-EXIT.
     EXIT.
******************************************************************
*    商品在庫マスタ読込み (降順）
******************************************************************
 RD-ZAMZAIF-BWD-SEC    SECTION.
     IF  FG-ZAMZAIF-RVS-END = LOW-VALUE
         MOVE  SPACE        TO  FG-ZAMZAIF-RVS-END

         IF  FG-ZAMZAIF-RVS-START = SPACE
             START  ZAMZAIF  KEY <= ZAI-F02
                                    ZAI-F01
                                    ZAI-F03
                      WITH  REVERSED ORDER
               INVALID KEY
                 MOVE  "END"   TO  FG-ZAMZAIF-RVS-END
                 GO TO  RD-ZAMZAIF-BWD-EXIT
             END-START
         ELSE
             MOVE  SPACE    TO  FG-ZAMZAIF-RVS-START

             START  ZAMZAIF  KEY < ZAI-F02
                                   ZAI-F01
                                   ZAI-F03
                      WITH  REVERSED ORDER
               INVALID KEY
                 MOVE  "END"   TO  FG-ZAMZAIF-RVS-END
                 GO TO  RD-ZAMZAIF-BWD-EXIT
             END-START
         END-IF

     END-IF.

     READ  ZAMZAIF  NEXT
       AT END
         MOVE  "END"        TO  FG-ZAMZAIF-RVS-END
         GO TO  RD-ZAMZAIF-BWD-EXIT
     END-READ.

     IF  ZAI-F021 < DSP-SHOCD *> 商品ＣＤ
         MOVE  "END"        TO  FG-ZAMZAIF-RVS-END
         GO TO  RD-ZAMZAIF-BWD-EXIT
     END-IF.

     IF       ZAI-F021 = DSP-SHOCD
         AND  ZAI-F022 < WK-SHT-HINTAN *> 品単
         MOVE  "END"        TO  FG-ZAMZAIF-RVS-END
         GO TO  RD-ZAMZAIF-BWD-EXIT
     END-IF.

     IF  DSP-SOKCD = SPACE
         CONTINUE
     ELSE
         IF  ZAI-F01 < DSP-SOKCD *> 倉庫ＣＤ
             GO TO  RD-ZAMZAIF-BWD-SEC
         END-IF
     END-IF.

 RD-ZAMZAIF-BWD-EXIT.
     EXIT.
******************************************************************
*  次レコード検索処理
******************************************************************
 FWD-REC-SEC           SECTION.

* 次レコード検索
     MOVE  LOW-VALUE        TO  FG-SHOTBL-END.

 FWD-REC-010.

     IF  DSP-RYSHCD NOT = WK-SHOTBL-STKY-AITSHO
         INITIALIZE  SHT-REC
         MOVE  DSP-RYSHCD       TO  SHT-F02  *>相手先商品CD
     ELSE   *> 前回キーを設定
         MOVE  WK-SHOTBL-STKY-AITSHO  TO  SHT-F02
         MOVE  WK-SHOTBL-STKY-SHOCD   TO  SHT-F031
         MOVE  WK-SHOTBL-STKY-HINTAN  TO  SHT-F032
     END-IF.

     MOVE  ">"              TO  FG-SHOTBL-START.
     PERFORM  RD-SHOTBL-FWD-SEC.
     IF  FG-SHOTBL-END = "END"
         IF  ERR-NO = ZERO
             MOVE  9        TO  ERR-NO
         END-IF

         *> 退避したヘッド情報より復元
         MOVE  SV-DSP-RYSHCD  TO  DSP-RYSHCD
         MOVE  SV-DSP-SHOCD   TO  DSP-SHOCD
         MOVE  SV-DSP-KANANM  TO  DSP-KANANM
         MOVE  SV-DSP-SHONM   TO  DSP-SHONM
         GO TO  FWD-REC-EXIT
     END-IF.

  *> 商品変換ＴＢＬレコード開始キー領域
     MOVE  SHT-F02          TO  WK-SHOTBL-STKY-AITSHO.
     MOVE  SHT-F031         TO  WK-SHOTBL-STKY-SHOCD.
     MOVE  SHT-F032         TO  WK-SHOTBL-STKY-HINTAN.

* 次レコード相手先商品ＣＤの画面更新
     MOVE  SHT-F02          TO  DSP-RYSHCD. *> 相手先商品ＣＤ
     MOVE  SHT-F031         TO  DSP-SHOCD.  *> 商品ＣＤ
     MOVE  DSP-SHOCD        TO  WK-SHOCD.
     PERFORM  SHOCD-CV-SEC.
     MOVE  WK-SHOCD         TO  DSP-SHOCD.

  *> 検索品単の設定
     MOVE  SHT-F032         TO  WK-SHT-HINTAN.

     MOVE  SPACE            TO  DSP-SHONM.
     MOVE  SPACE            TO  DSP-KANANM.

     IF  SYORI-GROUP = "HEAD" *> ヘッド部の場合
         PERFORM  HD-CHK-SEC
         IF  ERR-NO NOT = ZERO
             GO TO  FWD-REC-EXIT
         END-IF
     ELSE                     *> 明細部の場合
         INITIALIZE  ZAI-REC
         MOVE  SHT-F03      TO  ZAI-F02 *> 商品ＣＤ
         MOVE  DSP-SOKCD    TO  ZAI-F01 *> 倉庫ＣＤ

         *> 在庫マスタ合計検索開始キー設定
         MOVE  ZAI-F021     TO  WK-TOTST-KEY-SHOCD
         MOVE  ZAI-F022     TO  WK-TOTST-KEY-HINTAN
         MOVE  ZAI-F01      TO  WK-TOTST-KEY-SOKCD
         MOVE  ZAI-F03      TO  WK-TOTST-KEY-TANANO

         MOVE  LOW-VALUE    TO  FG-ZAMZAIF-END
         PERFORM  RD-ZAMZAIF-FWD-SEC
         IF  FG-ZAMZAIF-END = "END"
             GO TO  FWD-REC-010
         END-IF
     END-IF.

     MOVE  ZAI-F021         TO  MEI-F011. *> 商品ＣＤ
     MOVE  ZAI-F022         TO  MEI-F012. *> 品単
     PERFORM  RD-HMEIMS-SEC.
     IF  FG-HMEIMS-INV = 1
         MOVE  ALL NC"＊"   TO  DSP-SHONM
     ELSE
         MOVE  MEI-F02      TO  DSP-SHONM
     END-IF.

     MOVE  ZAI-F30          TO  DSP-KANANM.
   *> レコードなし時のために退避する。
     MOVE  DSP-RYSHCD       TO  SV-DSP-RYSHCD.
     MOVE  DSP-SHOCD        TO  SV-DSP-SHOCD.
     MOVE  DSP-KANANM       TO  SV-DSP-KANANM.
     MOVE  DSP-SHONM        TO  SV-DSP-SHONM.

* 次レコードの画面編集
     MOVE "RC"              TO  FG-RCD-PAG.
     PERFORM  GMN-MSEDT-SEC.

 FWD-REC-EXIT.
     EXIT.
******************************************************************
*             ボディ処理
******************************************************************
 BODY-SEC               SECTION.
* ボディ部入力

     PERFORM  DSP-WT-SEC.

     EVALUATE DSP-FNC
       WHEN  "F005" *> 終了
         MOVE  9            TO  END-FLG
*********MOVE  4010         TO  PROGRAM-STATUS

       WHEN  "F004" *> 取消
         PERFORM  DSP-INIT-SEC
         MOVE  "HEAD"       TO  SYORI-GROUP

       WHEN  "E000" *> 処理
         CONTINUE

       WHEN  "F007" *> 前頁
         PERFORM  BWD-PAGE-SEC

       WHEN  "F008" *> 次頁
         PERFORM  FWD-PAGE-SEC

       WHEN  "F011" *> 前レコード
         PERFORM  BWD-REC-SEC

       WHEN  "F012" *> 次レコード
         PERFORM  FWD-REC-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-NO

     END-EVALUATE.

 BODY-EXIT.
     EXIT.
******************************************************************
*  前頁検索処理
******************************************************************
 BWD-PAGE-SEC           SECTION.
* 前頁データ検索
     INITIALIZE  ZAI-REC.
     MOVE  WK-PGST-KEY-SHOCD-G   TO  ZAI-F02. *> 商品ＣＤ
     MOVE  WK-PGST-KEY-SOKCD     TO  ZAI-F01. *> 倉庫ＣＤ
     MOVE  WK-PGST-KEY-TANANO    TO  ZAI-F03. *> _番

     MOVE  LOW-VALUE        TO  FG-ZAMZAIF-RVS-END.
     MOVE  "<"              TO  FG-ZAMZAIF-RVS-START.
     PERFORM  RD-ZAMZAIF-BWD-SEC.
     IF  FG-ZAMZAIF-RVS-END = "END"
         IF  ERR-NO = ZERO
             MOVE  6        TO  ERR-NO
         END-IF
         GO TO  BWD-PAGE-EXIT
     END-IF.

* 前頁データの画面編集
     MOVE "PG"              TO  FG-RCD-PAG-RVS.
     PERFORM  GMN-MSEDT-RVS-SEC.

 BWD-PAGE-EXIT.
     EXIT.
******************************************************************
*    画面編集（前方向)
******************************************************************
 GMN-MSEDT-RVS-SEC     SECTION.
     MOVE  ZERO             TO  IX-GYO.
     IF  DSP-SOKCD = SPACE
         *> 合計行作成
         PERFORM  GMN-MSEDT-TOT-SEC

     END-IF.

     PERFORM  GMN-MSEDTB-RVS-SEC.

 GMN-MSEDT-EXIT.
     EXIT.
******************************************************************
*    画面編集Ｂ（前方向）
******************************************************************
 GMN-MSEDTB-RVS-SEC    SECTION.

     IF  DSP-SOKCD = SPACE *> 合計集計の場合再度読込む
         IF  FG-RCD-PAG-RVS = "RC" *> 前レコード処理
             INITIALIZE  ZAI-REC
             MOVE  SHT-F03          TO  ZAI-F02 *> 商品ＣＤ
             IF  DSP-SOKCD NOT = SPACE
                 MOVE  DSP-SOKCD    TO  ZAI-F01 *> 倉庫ＣＤ
             ELSE
                 MOVE  HIGH-VALUE   TO  ZAI-F01 *> 倉庫ＣＤ
             END-IF
             MOVE  HIGH-VALUE       TO  ZAI-F03 *> _番
             MOVE  LOW-VALUE        TO  FG-ZAMZAIF-RVS-END

         ELSE               *> 前頁処理
             INITIALIZE  ZAI-REC
             MOVE  WK-PGST-KEY-SHOCD-G TO  ZAI-F02 *> 商品ＣＤ
             MOVE  WK-PGST-KEY-SOKCD   TO  ZAI-F01 *> 倉庫ＣＤ
             MOVE  WK-PGST-KEY-TANANO  TO  ZAI-F03 *> _番

             MOVE  LOW-VALUE        TO  FG-ZAMZAIF-RVS-END
             MOVE  "<"              TO  FG-ZAMZAIF-RVS-START
         END-IF

         PERFORM  RD-ZAMZAIF-BWD-SEC
         IF  FG-ZAMZAIF-RVS-END = "END"
             GO TO  GMN-MSEDTB-RVS-EXIT
         END-IF

     END-IF.

  *> 頁制御キーの初期化
     INITIALIZE  WK-PGST-KEY.
     INITIALIZE  WK-PGED-KEY.

* データ行の編集
     COMPUTE  IX-GYO-ST = IX-GYO + 1. *> 格納先頭行の計算
     MOVE  ZERO             TO  IX-GYO-ST2.
     MOVE  8                TO  IX-GYO.

     PERFORM  UNTIL   IX-GYO <= IX-GYO-ST
                   OR FG-ZAMZAIF-RVS-END = "END"
       COMPUTE  IX-GYO = IX-GYO - 1
       PERFORM  GMN-MSEDT-PGKYSV-RVS-SEC
       PERFORM  GMN-MSEDTC-SEC
       MOVE  IX-GYO         TO  IX-GYO-ST2
       PERFORM  RD-ZAMZAIF-BWD-SEC

     END-PERFORM.

* 空白行の編集
     IF  IX-GYO-ST2 = IX-GYO-ST + 1 *> 頁の先頭まである
         CONTINUE
     ELSE
         *> 先頭行と設定先頭行の差を計算する。
         COMPUTE  WK-GYO-SA = IX-GYO-ST - IX-GYO-ST2
         *> 前空き行を先頭行からの行に移動させる。
         PERFORM  VARYING IX-GYO  FROM IX-GYO-ST2 BY 1
                  UNTIL   IX-GYO > 7

           COMPUTE  IX-GYO2 = IX-GYO - WK-GYO-SA *> 移動先行
           MOVE  DSP-BODY01 (IX-GYO)
                            TO  DSP-BODY01 (IX-GYO2)

         END-PERFORM

         *> 移動させた空き行を空白にする。
         COMPUTE  IX-GYO2 = 7 - WK-GYO-SA + 1 *> 空白先頭行
         PERFORM  VARYING IX-GYO  FROM IX-GYO2 BY 1
                  UNTIL   IX-GYO  > 7
           MOVE  SPACE      TO  DSP-BODY01 (IX-GYO)

         END-PERFORM
     END-IF.

 GMN-MSEDTB-RVS-EXIT.
     EXIT.
******************************************************************
*    画面頁制御キー退避（前方向）
******************************************************************
 GMN-MSEDT-PGKYSV-RVS-SEC  SECTION.
* 頁開始キーの退避
     MOVE  ZAI-F02          TO  WK-PGST-KEY-SHOCD-G. *>商品ＣＤ
     MOVE  ZAI-F01          TO  WK-PGST-KEY-SOKCD.   *>倉庫ＣＤ
     MOVE  ZAI-F03          TO  WK-PGST-KEY-TANANO.  *>_番
* 頁終了キーの退避
     IF  IX-GYO = 7
         MOVE  ZAI-F02      TO  WK-PGED-KEY-SHOCD-G  *> 商品ＣＤ
         MOVE  ZAI-F01      TO  WK-PGED-KEY-SOKCD    *> 倉庫ＣＤ
         MOVE  ZAI-F03      TO  WK-PGED-KEY-TANANO   *> _番
     END-IF.

 GMN-MSEDT-PGKYSV-RVS-EXIT.
     EXIT.
******************************************************************
*  次頁検索処理
******************************************************************
 FWD-PAGE-SEC           SECTION.
* 次頁データ検索
     INITIALIZE  ZAI-REC.
     MOVE  WK-PGED-KEY-SHOCD-G   TO  ZAI-F02. *> 商品ＣＤ
     MOVE  WK-PGED-KEY-SOKCD     TO  ZAI-F01. *> 倉庫ＣＤ
     MOVE  WK-PGED-KEY-TANANO    TO  ZAI-F03. *> _番

     MOVE  LOW-VALUE        TO  FG-ZAMZAIF-END.
     MOVE  ">"              TO  FG-ZAMZAIF-START.
     PERFORM  RD-ZAMZAIF-FWD-SEC.
     IF  FG-ZAMZAIF-END = "END"
         IF  ERR-NO = ZERO
             MOVE  7        TO  ERR-NO
         END-IF
         GO TO  FWD-PAGE-EXIT
     END-IF.

* 次頁データの画面編集
     MOVE "PG"              TO  FG-RCD-PAG.
     PERFORM  GMN-MSEDT-SEC.

 FWD-PAGE-EXIT.
     EXIT.
******************************************************************
*             終了処理
******************************************************************
 END-SEC                SECTION.
     CLOSE  ZAMZAIF.
     CLOSE  HMEIMS.
     CLOSE  HJYOKEN.
     CLOSE  ZSOKMS.
     CLOSE  SHOTBL.
     CLOSE  TOKMS.
     CLOSE  DSPFILE.
 END-EXIT.
     EXIT.
