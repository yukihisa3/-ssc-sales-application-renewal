# SJS0500I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJS0500I.COB`

## ソースコード

```cobol
***************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　実績管理システム
*    モジュール名　　　　：　新受払照会
*    処理概要　　　　　　：　商品の受払いを、実績累積データより
*    　　　　　　　　　　　　照会する。
*    作成日／作成者　　　：　2020/05/26 INOUE
*    更新日／更新者　　　：
***************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SJS0500I.
*                     流用:SJS0101I.TOKSLIBS
 AUTHOR.                   Y.Y.
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
     SELECT      RUISEKL3  ASSIGN    TO        DA-01-VI-RUISEKL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       RUI-F10
                                               RUI-F11
                                               RUI-F17
                                               RUI-F09
                           FILE      STATUS    RUI-ST   RUI-ST1.
*実績累積ファイル
     SELECT      RUISEKL6  ASSIGN    TO        DA-01-VI-RUISEKL6
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       RUI6-F23
                                               RUI6-F17
                                               RUI6-F09
                           FILE      STATUS    RUI6-ST   RUI6-ST1.
*受払照会ワークＦ
     SELECT      NUKEBWK1  ASSIGN    TO        DA-01-VI-NUKEBWK1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       UKE-F01
                           FILE      STATUS    UKE-ST   UKE-ST1.
*商品コード変換テーブル
     SELECT      SHOTBL7   ASSIGN    TO        DA-01-VI-SHOTBL7
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SHO-F02
                           FILE      STATUS    SHO-ST   SHO-ST1.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011
                                               MEI-F012
                           FILE      STATUS    MEI-ST    MEI-ST1.
*サブ商品名称マスタ
     SELECT      SUBMEIL1  ASSIGN    TO        DA-01-VI-SUBMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SUB-F011
                                               SUB-F0121
                                               SUB-F0122
                                               SUB-F0123
                           FILE      STATUS    SUB-ST    SUB-ST1.
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
*取引先マスタ
     SELECT      TOKMS3    ASSIGN    TO        DA-01-VI-TOKMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F52
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
 FD  RUISEKL3.
     COPY        RUISEKF   OF        XFDLIB
     JOINING     RUI       AS        PREFIX.
*実績累積ファイル
 FD  RUISEKL6.
     COPY        RUISEKF   OF        XFDLIB
     JOINING     RUI6      AS        PREFIX.
*受払照会ワークＦ
 FD  NUKEBWK1.
     COPY        NUKEBWK1  OF        XFDLIB
     JOINING     UKE       AS        PREFIX.
*商品変換テーブル
 FD  SHOTBL7.
     COPY        SHOTBL7   OF        XFDLIB
     JOINING     SHO       AS        PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL1.
     COPY        SUBMEIF   OF        XFDLIB
                 JOINING   SUB       PREFIX.
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
     COPY        FJS05001  OF        XMDLIB.
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
     03  MSG12             PIC N(20) VALUE
                     NC"商品コードを入力して下さい".
     03  MSG13             PIC N(20) VALUE
                     NC"商品名称マスタに存在しません".
     03  MSG14             PIC N(20) VALUE
                     NC"場所を入力して下さい".
     03  MSG15             PIC N(20) VALUE
                     NC"日付に誤りがあります".
     03  MSG16             PIC N(20) VALUE
                     NC"区分に誤りがあります".
     03  MSG17             PIC N(20) VALUE
                     NC"サブ商品名称マスタに存在しません".
     03  MSG18             PIC N(20) VALUE
                     NC"商品変換テーブルに存在しません".
 01  MSG-AREAR             REDEFINES      MSG-AREA.
     03  MSGIX             PIC N(20)      OCCURS   18.
*
 01  PFKEY-MSG.
     03  PMSG00            PIC N(30) VALUE
       NC"_取消　_終了".
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
*画面表示ＩＤ
 01  WRK-PGID                     PIC  X(08)  VALUE  "SJS0500I".
 01  WRK-FORMID                   PIC  X(08)  VALUE  "FJS05001".
*特販部名称編集
*01  HEN-TOKHAN-AREA.
*    03  FILLER                   PIC  N(01)  VALUE  NC"（".
*    03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
*    03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  SYORI-GROUP       PIC X(06).
     03  WK-MEI-F02G.
       05  WK-MEI-F02      PIC  N(30).
     03  ERR-NO            PIC  9(02)  VALUE  ZERO.
     03  WK-HIZUKE         PIC  9(08)  VALUE  ZERO.
 01  CNT-AREA.
     03  CNT-SEQ           PIC  9(08)  VALUE  ZERO.
     03  WK-SEQ            PIC  9(08)  VALUE  ZERO.
     03  CNT-PAGE          PIC  9(08)  VALUE  ZERO.
     03  CNT-UKEBWKF       PIC  9(08)  VALUE  ZERO.
     03  MAX-PAGE          PIC  9(08)  VALUE  ZERO.
     03  WK-AMARI          PIC  9(02)  VALUE  ZERO.
     03  RUISEKF-FLG       PIC  X(03)  VALUE  SPACE.
     03  RUISEK6-FLG       PIC  X(03)  VALUE  SPACE.
     03  UKEBWKF-FLG       PIC  X(03)  VALUE  SPACE.
     03  MEIMS-FLG         PIC  X(03)  VALUE  SPACE.
     03  SUBMEI-FLG        PIC  X(03)  VALUE  SPACE.
     03  SHOTBL-FLG        PIC  X(03)  VALUE  SPACE.
     03  RUI-REC-INV       PIC  X(03)  VALUE  SPACE.
     03  RUI6-REC-INV      PIC  X(03)  VALUE  SPACE.
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
     03  RUI6-ST           PIC X(02).
     03  RUI6-ST1          PIC X(04).
     03  UKE-ST            PIC X(02).
     03  UKE-ST1           PIC X(04).
     03  SHO-ST            PIC X(02).
     03  SHO-ST1           PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  SUB-ST            PIC X(02).
     03  SUB-ST1           PIC X(04).
     03  JYKN-ST           PIC X(02).
     03  JYKN-ST1          PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
     03  KBM-ST            PIC X(02).
     03  KBM-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
*-------------------------
*
     03  FILE-ERR1         PIC N(20) VALUE
                     NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(20) VALUE
                     NC"実績累積ファイル３異常！".
     03  FILE-ERR3         PIC N(20) VALUE
                     NC"受払照会ワークＦ異常！".
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
                     NC"実績累積ファイル６異常！".
     03  FILE-ERR10        PIC N(20) VALUE
                     NC"サブ商品名称マスタ異常！".
     03  FILE-ERR11        PIC N(20) VALUE
                     NC"商品変換テーブル異常！".
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
 RUI-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         RUISEKL3.
     DISPLAY       RUI-ST    UPON      STA.
     DISPLAY       RUI-ST1   UPON      STA.
     DISPLAY       FILE-ERR2 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 RUI6-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         RUISEKL6.
     DISPLAY       RUI6-ST   UPON      STA.
     DISPLAY       RUI6-ST1  UPON      STA.
     DISPLAY       FILE-ERR9 UPON      STA.
     ACCEPT        IN-DATA   FROM      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP          RUN.
 UKE-ERR               SECTION.
     USE           AFTER     EXCEPTION PROCEDURE         NUKEBWK1.
     DISPLAY       UKE-ST    UPON      STA.
     DISPLAY       UKE-ST1   UPON      STA.
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
 SUB-ERR                SECTION.
     USE           AFTER      EXCEPTION PROCEDURE        SUBMEIL1.
     DISPLAY       SUB-ST     UPON      STA.
     DISPLAY       SUB-ST1    UPON      STA.
     DISPLAY       FILE-ERR10 UPON      STA.
     ACCEPT        IN-DATA    FROM      STA.
     MOVE          4000       TO        PROGRAM-STATUS.
     STOP          RUN.
 SHO-ERR                SECTION.
     USE           AFTER      EXCEPTION PROCEDURE        SHOTBL7.
     DISPLAY       SHO-ST     UPON      STA.
     DISPLAY       SHO-ST1    UPON      STA.
     DISPLAY       FILE-ERR11 UPON      STA.
     ACCEPT        IN-DATA    FROM      STA.
     MOVE          4000       TO        PROGRAM-STATUS.
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
     OPEN     INPUT     RUISEKL3
                        RUISEKL6
                        HMEIMS
                        SUBMEIL1
                        SHOTBL7
                        HJYOKEN
                        ZSOKMS
                        SGYKBMF
                        TOKMS3
              I-O       DSPFILE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL    "SKYDTCKB"        USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   DATE-AREA.
*画面表示日付編集
     MOVE     SYS-DATE(1:4)       TO   HEN-DATE-YYYY.
     MOVE     SYS-DATE(5:2)       TO   HEN-DATE-MM.
     MOVE     SYS-DATE(7:2)       TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT   WK-TIME           FROM   TIME.
*画面表示時刻編集
     MOVE     WK-TIME(1:2)        TO   HEN-TIME-HH.
     MOVE     WK-TIME(3:2)        TO   HEN-TIME-MM.
     MOVE     WK-TIME(5:2)        TO   HEN-TIME-SS.
*特販部名称編集
*    MOVE     SPACE               TO   JYOKEN-REC.
*    INITIALIZE                        JYOKEN-REC.
*    MOVE    "99"                 TO   JYOKEN-F01.
*    MOVE    "BUMON"              TO   JYOKEN-F02.
*    READ     HJYOKEN
*      INVALID KEY
*             MOVE ALL NC"＊"     TO   HEN-TOKHAN
*      NOT INVALID KEY
*             MOVE JYOKEN-F03     TO   HEN-TOKHAN
*    END-READ.
*
     PERFORM            DSP-INIT-SEC.
     MOVE    "SYKKBN"             TO   SYORI-GROUP.
 INIT-EXIT.
     EXIT.
******************************************************************
*             初期画面　表示
******************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE     SPACE               TO   FJS05001.
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
     MOVE     WRK-PGID            TO   PGID.
     MOVE     WRK-FORMID          TO   FORMID.
*    MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
*
     IF       LINK-DSOKCD    =   "01"
              CONTINUE
     ELSE
              MOVE   LINK-SOKCD   TO   SOKCD  SOK-F01
              READ      ZSOKMS
                INVALID
                   CONTINUE
                NOT  INVALID
                   MOVE  SOK-F02  TO   SOKNM
              END-READ
     END-IF.
*
     MOVE    "FJS05001"           TO   DSP-FMT.
     MOVE    "SCREEN"             TO   DSP-GRP.
     MOVE    "CL"                 TO   DSP-PRO.
     WRITE    FJS05001.
 DSP-INIT-EXIT.
     EXIT.
******************************************************************
*             主処理
******************************************************************
 MAIN-SEC               SECTION.
     MOVE     ZERO                TO   ERR-NO.
*T
*    DISPLAY "SYORI-GROUP=" SYORI-GROUP
*    STOP RUN
*T
     EVALUATE      SYORI-GROUP
       WHEN       "SYKKBN"
                   PERFORM        KBN-SEC
       WHEN       "HEAD"
                   PERFORM        HEAD-SEC
       WHEN       "HEAD1"
                   PERFORM        HEAD1-SEC
       WHEN       "BODY"
                   PERFORM        BODY-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
******************************************************************
*             区分指定（サカタ商品指定OR相手商品指定）
******************************************************************
 KBN-SEC               SECTION.
*ヘッド部入力
     PERFORM            PFK-DSP-SEC.
*    IF       LINK-DSOKCD    =   "01"
*             CONTINUE
*    ELSE
*             MOVE  "X"           TO   EDIT-STATUS  OF  SOKCD
*    END-IF.
     MOVE    "SYKKBN"             TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN    "F004"
              MOVE      SPACE     TO   FJS05001
              MOVE     "SCREEN"   TO   DSP-GRP
              PERFORM   EDIT-SET-HEAD
              PERFORM   DSP-WR-SEC
                                  GO   TO   KBN-EXIT
     WHEN    "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   KBN-EXIT
     WHEN    "E000"
              PERFORM   EDIT-SET-KBN
     WHEN     OTHER
              MOVE   1            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   KBN-EXIT
     END-EVALUATE.
*条件 CHECK
     MOVE     ZERO      TO   ERR-NO.
*    区分
     IF  ( SYKKBN    NOT =     "1" ) AND
         ( SYKKBN    NOT =     "2" )
         IF   ERR-NO     =     ZERO
              MOVE      16     TO    ERR-NO
              MOVE     "C"     TO    EDIT-CURSOR  OF  SYKKBN
         END-IF
         MOVE          "R"     TO    EDIT-OPTION  OF  SYKKBN
     ELSE
         IF   SYKKBN     =     "1"
              MOVE     "HEAD"  TO    SYORI-GROUP
         ELSE
              MOVE     "HEAD1" TO    SYORI-GROUP
         END-IF
     END-IF.
*
     MOVE    "SYKKBN"             TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    0
              MOVE  SYORI-GROUP   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
     END-IF.
 KBN-EXIT.
     EXIT.
******************************************************************
*             ヘッド処理（サカタ商品指定）
******************************************************************
 HEAD-SEC               SECTION.
*ヘッド部入力
     PERFORM            PFK-DSP-SEC.
     IF       LINK-DSOKCD    =   "01"
              CONTINUE
     ELSE
              MOVE  "X"           TO   EDIT-STATUS  OF  SOKCD
     END-IF.
     MOVE    "HEAD"               TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F004"
              MOVE      SPACE     TO   FJS05001
              MOVE     "SCREEN"   TO   DSP-GRP
              PERFORM   EDIT-SET-HEAD
              PERFORM   DSP-WR-SEC
              MOVE     "SYKKBN"   TO   SYORI-GROUP
                                  GO   TO   HEAD-EXIT
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   HEAD-EXIT
     WHEN         "F007"
              PERFORM   EDIT-SET-HEAD
              MOVE      SPACE     TO   BODYG
              PERFORM   BEFORE-REC-SEC
                                  GO   TO   HEAD-EXIT
     WHEN         "F008"
              PERFORM   EDIT-SET-HEAD
              MOVE      SPACE     TO   BODYG
              PERFORM   NEXT-REC-SEC
                                  GO   TO   HEAD-EXIT
     WHEN         "E000"
              PERFORM   EDIT-SET-HEAD
     WHEN          OTHER
              MOVE   1            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   HEAD-EXIT
     END-EVALUATE.
*検索条件 CHECK
     MOVE     ZERO      TO   ERR-NO.
*    商品コード
     IF  SHOCD     =    SPACE
         IF   ERR-NO    =    ZERO
              MOVE      12   TO   ERR-NO
              MOVE     "C"   TO   EDIT-CURSOR  OF  SHOCD
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  SHOCD
     ELSE
         MOVE      SHOCD     TO   WK-SHOCD
         PERFORM        UNTIL     WK-SHO(8) NOT = SPACE
              PERFORM VARYING IXA FROM 7 BY -1 UNTIL IXA = 0
                   MOVE      WK-SHO(IXA)    TO  WK-SHO(IXA + 1)
              END-PERFORM
              MOVE      ZERO      TO  WK-SHO(1)
         END-PERFORM
         MOVE      WK-SHOCD       TO   SHOCD
     END-IF.
*    品単
     MOVE     SPACE     TO   WK-HINT.
     MOVE     HIN1      TO   WK-HIN1.
     MOVE     HIN2      TO   WK-HIN2.
     MOVE     HIN3      TO   WK-HIN3.
*    商品名称マスタ検索
     MOVE     SPACE     TO   MEI-REC.
     INITIALIZE              MEI-REC.
     MOVE     SHOCD     TO   MEI-F011.
     MOVE     HIN1      TO   MEI-F0121.
     MOVE     HIN2      TO   MEI-F0122.
     MOVE     HIN3      TO   MEI-F0123.
     IF       SHOCD     NOT =     SPACE
              PERFORM   MEIMS-READ-SEC
              IF   MEIMS-FLG      =   "INV"
                   IF   ERR-NO    =    ZERO
                        MOVE      13   TO  ERR-NO
                        MOVE     "C"   TO  EDIT-CURSOR  OF  SHOCD
                   END-IF
                   MOVE     "R"   TO   EDIT-OPTION  OF  SHOCD
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN1
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN2
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN3
              END-IF
     END-IF.
*    サブ商品名称マスタ検索
     MOVE     SPACE     TO   SUB-REC.
     INITIALIZE              SUB-REC.
     MOVE     SHOCD     TO   SUB-F011.
     MOVE     HIN1      TO   SUB-F0121.
     MOVE     HIN2      TO   SUB-F0122.
     MOVE     HIN3      TO   SUB-F0123.
     IF       SHOCD     NOT =     SPACE
              PERFORM   SUBMEIF-READ-SEC
              IF   SUBMEI-FLG     =   "INV"
                   IF   ERR-NO    =    ZERO
                        MOVE      17   TO  ERR-NO
                        MOVE     "C"   TO  EDIT-CURSOR  OF  SHOCD
                   END-IF
                   MOVE     "R"   TO   EDIT-OPTION  OF  SHOCD
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN1
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN2
                   MOVE     "R"   TO   EDIT-OPTION  OF  HIN3
              END-IF
     END-IF.
*倉庫コード
     IF  LINK-DSOKCD    =   "01"
         MOVE      SPACE     TO   SOKNM
         IF   SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        IF   ERR-NO    =    ZERO
                             MOVE  10  TO   ERR-NO
                             MOVE "C"  TO   EDIT-CURSOR OF SOKCD
                        END-IF
                        MOVE      "R"  TO   EDIT-OPTION OF SOKCD
                   NOT INVALID
                        MOVE SOK-F02   TO  SOKNM
              END-READ
         ELSE
              IF   ERR-NO    =    ZERO
                   MOVE      14   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF  SOKCD
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         END-IF
     END-IF.
*処理日
     IF     ( HIZUKE    NUMERIC   )    AND
            ( HIZUKE    >    ZERO )
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE      HIZUKE         TO   LINK-IN-YMD6
         MOVE      ZERO           TO   LINK-IN-YMD8
         MOVE      ZERO           TO   LINK-OUT-RET
         MOVE      ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         IF   LINK-OUT-RET NOT =  ZERO
              IF   ERR-NO    =    ZERO
                   MOVE      15   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF HIZUKE
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF HIZUKE
         ELSE
              MOVE      LINK-OUT-YMD   TO   WK-HIZUKE
         END-IF
     ELSE
         MOVE      ZERO      TO   WK-HIZUKE
     END-IF.
*    受払照会ワークＦ出力
     IF       ERR-NO    =    ZERO
              OPEN      OUTPUT    NUKEBWK1
              PERFORM   UKEBWKF-WRITE-SEC
              CLOSE     NUKEBWK1
     END-IF.
*
     IF       CNT-UKEBWKF    =    ZERO
              IF   ERR-NO    =    ZERO
                   MOVE      11   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF SHOCD
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF SHOCD
              MOVE     "R"   TO   EDIT-OPTION  OF HIN1
              MOVE     "R"   TO   EDIT-OPTION  OF HIN2
              MOVE     "R"   TO   EDIT-OPTION  OF HIN3
              MOVE     "R"   TO   EDIT-OPTION  OF SOKCD
     ELSE
              IF   CNT-UKEBWKF    <=   14
                   MOVE      1    TO   MAX-PAGE
              ELSE
                   DIVIDE    CNT-UKEBWKF   BY   14
                        GIVING         MAX-PAGE
                        REMAINDER      WK-AMARI
                   IF   WK-AMARI  >    ZERO
                        ADD  1    TO   MAX-PAGE
                   END-IF
              END-IF
     END-IF.
     IF       ERR-NO    =    ZERO
              MOVE      1    TO   CNT-PAGE
              PERFORM   BODY-SET-SEC
              MOVE     "BODY"     TO   SYORI-GROUP
     END-IF.
*
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
*             ヘッド処理（相手商品指定）
******************************************************************
 HEAD1-SEC               SECTION.
*ヘッド部入力
     PERFORM            PFK-DSP-SEC.
     IF       LINK-DSOKCD    =   "01"
              CONTINUE
     ELSE
              MOVE  "X"           TO   EDIT-STATUS  OF  SOKCD
     END-IF.
     MOVE    "HEAD1"              TO   DSP-GRP.
     PERFORM       DSP-RD-SEC.
     EVALUATE      DSP-FNC
     WHEN         "F004"
              MOVE      SPACE     TO   FJS05001
              MOVE     "SCREEN"   TO   DSP-GRP
              PERFORM   EDIT-SET-HEAD
              PERFORM   DSP-WR-SEC
              MOVE     "SYKKBN"   TO   SYORI-GROUP
                                  GO   TO   HEAD1-EXIT
     WHEN         "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   HEAD1-EXIT
     WHEN         "F007"
              PERFORM   EDIT-SET-HEAD
              MOVE      SPACE     TO   BODYG
              PERFORM   BEFORE6-REC-SEC
                                  GO   TO   HEAD1-EXIT
     WHEN         "F008"
              PERFORM   EDIT-SET-HEAD
              MOVE      SPACE     TO   BODYG
              PERFORM   NEXT6-REC-SEC
                                  GO   TO   HEAD1-EXIT
     WHEN         "E000"
              PERFORM   EDIT-SET-HEAD
     WHEN          OTHER
              MOVE   1            TO   ERR-NO
              PERFORM   ERR-DSP-SEC
                                  GO   TO   HEAD1-EXIT
     END-EVALUATE.
*検索条件 CHECK
     MOVE     ZERO      TO   ERR-NO.
*    相手商品コード
     IF  AITECD    =    SPACE
         IF   ERR-NO    =    ZERO
              MOVE      12   TO   ERR-NO
              MOVE     "C"   TO   EDIT-CURSOR  OF  AITECD
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
     END-IF.
*    商品変換テーブル検索
     MOVE     SPACE     TO   SHO-REC.
     INITIALIZE              SHO-REC.
     MOVE     AITECD    TO   SHO-F02.
     IF       AITECD    NOT =     SPACE
              PERFORM   SHOTBL-READ-SEC
              IF   SHOTBL-FLG      =   "INV"
                   IF   ERR-NO    =    ZERO
                        MOVE      18   TO  ERR-NO
                        MOVE     "C"   TO  EDIT-CURSOR  OF  AITECD
                   END-IF
                   MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
              END-IF
     END-IF.
*    サブ商品名称マスタ検索
     MOVE     SPACE     TO   SUB-REC.
     INITIALIZE              SUB-REC.
     MOVE     SHO-F031  TO   SUB-F011.
     MOVE     SHO-F0321 TO   SUB-F0121.
     MOVE     SHO-F0322 TO   SUB-F0122.
     MOVE     SHO-F0323 TO   SUB-F0123.
     IF       SHO-F031  NOT =     SPACE
              PERFORM   SUBMEIF-READ-SEC
              IF   SUBMEI-FLG     =   "INV"
                   IF   ERR-NO    =    ZERO
                        MOVE      17   TO  ERR-NO
                        MOVE     "C"   TO  EDIT-CURSOR  OF  AITECD
                   END-IF
                   MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
              END-IF
     END-IF.
*倉庫コード
     IF  LINK-DSOKCD    =   "01"
         MOVE      SPACE     TO   SOKNM
         IF   SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        IF   ERR-NO    =    ZERO
                             MOVE  10  TO   ERR-NO
                             MOVE "C"  TO   EDIT-CURSOR OF SOKCD
                        END-IF
                        MOVE      "R"  TO   EDIT-OPTION OF SOKCD
                   NOT INVALID
                        MOVE SOK-F02   TO  SOKNM
              END-READ
         ELSE
              IF   ERR-NO    =    ZERO
                   MOVE      14   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF  SOKCD
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         END-IF
     END-IF.
*処理日
     IF     ( HIZUKE    NUMERIC   )    AND
            ( HIZUKE    >    ZERO )
         MOVE     "3"             TO   LINK-IN-KBN
         MOVE      HIZUKE         TO   LINK-IN-YMD6
         MOVE      ZERO           TO   LINK-IN-YMD8
         MOVE      ZERO           TO   LINK-OUT-RET
         MOVE      ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         IF   LINK-OUT-RET NOT =  ZERO
              IF   ERR-NO    =    ZERO
                   MOVE      15   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF HIZUKE
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF HIZUKE
         ELSE
              MOVE      LINK-OUT-YMD   TO   WK-HIZUKE
         END-IF
     ELSE
         MOVE      ZERO      TO   WK-HIZUKE
     END-IF.
*    受払照会ワークＦ出力
     IF       ERR-NO    =    ZERO
              OPEN      OUTPUT    NUKEBWK1
              PERFORM   UKEBWKF-WRITE1-SEC
              CLOSE     NUKEBWK1
     END-IF.
*
     IF       CNT-UKEBWKF    =    ZERO
              IF   ERR-NO    =    ZERO
                   MOVE      11   TO   ERR-NO
                   MOVE     "C"   TO   EDIT-CURSOR  OF AITECD
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF AITECD
              MOVE     "R"   TO   EDIT-OPTION  OF SOKCD
     ELSE
              IF   CNT-UKEBWKF    <=   14
                   MOVE      1    TO   MAX-PAGE
              ELSE
                   DIVIDE    CNT-UKEBWKF   BY   14
                        GIVING         MAX-PAGE
                        REMAINDER      WK-AMARI
                   IF   WK-AMARI  >    ZERO
                        ADD  1    TO   MAX-PAGE
                   END-IF
              END-IF
     END-IF.
     IF       ERR-NO    =    ZERO
              MOVE      1    TO   CNT-PAGE
              PERFORM   BODY-SET-SEC
              MOVE     "BODY"     TO   SYORI-GROUP
     END-IF.
*
     MOVE    "HEAD1"              TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
     IF       ERR-NO              =    0
              MOVE  "BODY"        TO   DSP-GRP
              PERFORM   DSP-WR-SEC
     END-IF.
 HEAD1-EXIT.
     EXIT.
******************************************************************
*    受払照会ワークＦ出力処理（サカタ商品指定）
******************************************************************
 UKEBWKF-WRITE-SEC      SECTION.
     MOVE    "UKEBWKF-WRITE-SEC"       TO   DSP-GRP.
*
     MOVE     SPACE          TO   RUISEKF-FLG.
     MOVE     ZERO           TO   CNT-SEQ.
     MOVE     ZERO           TO   CNT-UKEBWKF.
*
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
     MOVE     SHOCD          TO   RUI-F10.
     MOVE     WK-HINT        TO   RUI-F11.
     MOVE     SOKCD          TO   RUI-F17.
     MOVE     WK-HIZUKE      TO   RUI-F09.
     START    RUISEKL3       KEY  >=   RUI-F10
                                       RUI-F11
                                       RUI-F17
                                       RUI-F09
         INVALID KEY
              GO   TO   UKEBWKF-WRITE-EXIT
     END-START.
*
 UKEBWKF-WRITE-010.
*
     READ     RUISEKL3  NEXT
         AT END
              GO   TO   UKEBWKF-WRITE-EXIT
         NOT AT END
              IF   RUI-F02  =  "30" OR "31" OR "32" OR "40" OR
                               "41" OR "50" OR "51" OR "70" OR
                               "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   UKEBWKF-WRITE-010
              END-IF
     END-READ.
     PERFORM  UNTIL     RUISEKF-FLG    =   "END"
         IF ( SHOCD     =    RUI-F10 ) AND
            ( WK-HINT   =    RUI-F11 ) AND
            ( SOKCD     =    RUI-F17 )
              IF   WK-HIZUKE <=   RUI-F09
                   MOVE      SPACE     TO   UKE-REC
                   INITIALIZE               UKE-REC
                   ADD       1         TO   CNT-SEQ
                   MOVE      CNT-SEQ   TO   UKE-F01
                   MOVE      RUI-F09   TO   UKE-F02
                   MOVE      RUI-F03   TO   UKE-F03
                   MOVE      RUI-F02   TO   UKE-F04
                   MOVE      RUI-F16   TO   UKE-F05
                   MOVE      RUI-F06   TO   UKE-F09
                   MOVE      RUI-F13   TO   UKE-F06
                   MOVE      RUI-F22   TO   UKE-F07
                   MOVE      RUI-F05   TO   UKE-F08
                   MOVE      RUI-F27   TO   UKE-F10
                   MOVE      RUI-F31   TO   UKE-F11
                   WRITE     UKE-REC
                   ADD       1         TO   CNT-UKEBWKF
              END-IF
*
              PERFORM   RUI-READ-SEC
         ELSE
              MOVE     "END"     TO   RUISEKF-FLG
         END-IF
     END-PERFORM.
*
 UKEBWKF-WRITE-EXIT.
     EXIT.
******************************************************************
*    受払照会ワークＦ出力処理（相手商品指定）
******************************************************************
 UKEBWKF-WRITE1-SEC      SECTION.
     MOVE    "UKEBWKF-WRITE1-SEC"       TO   DSP-GRP.
*
     MOVE     SPACE          TO   RUISEK6-FLG.
     MOVE     ZERO           TO   CNT-SEQ.
     MOVE     ZERO           TO   CNT-UKEBWKF.
*
     MOVE     SPACE          TO   RUI6-REC.
     INITIALIZE                   RUI6-REC.
     MOVE     AITECD         TO   RUI6-F23.
     MOVE     SOKCD          TO   RUI6-F17.
     MOVE     WK-HIZUKE      TO   RUI6-F09.
     START    RUISEKL6       KEY  >=   RUI6-F23
                                       RUI6-F17
                                       RUI6-F09
         INVALID KEY
              GO   TO   UKEBWKF-WRITE1-EXIT
     END-START.
*
 UKEBWKF-WRITE1-010.
*
     READ     RUISEKL6  NEXT
         AT END
              GO   TO   UKEBWKF-WRITE1-EXIT
         NOT AT END
              IF   RUI6-F02  =  "30" OR "31" OR "32" OR "40" OR
                               "41" OR "50" OR "51" OR "70" OR
                               "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   UKEBWKF-WRITE1-010
              END-IF
     END-READ.
     PERFORM  UNTIL     RUISEK6-FLG    =   "END"
         IF ( AITECD    =    RUI6-F23 ) AND
            ( SOKCD     =    RUI6-F17 )
              IF   WK-HIZUKE <=   RUI6-F09
                   MOVE      SPACE      TO   UKE-REC
                   INITIALIZE                UKE-REC
                   ADD       1          TO   CNT-SEQ
                   MOVE      CNT-SEQ    TO   UKE-F01
                   MOVE      RUI6-F09   TO   UKE-F02
                   MOVE      RUI6-F03   TO   UKE-F03
                   MOVE      RUI6-F02   TO   UKE-F04
                   MOVE      RUI6-F16   TO   UKE-F05
                   MOVE      RUI6-F06   TO   UKE-F09
                   MOVE      RUI6-F13   TO   UKE-F06
                   MOVE      RUI6-F22   TO   UKE-F07
                   MOVE      RUI6-F05   TO   UKE-F08
                   MOVE      RUI6-F27   TO   UKE-F10
                   MOVE      RUI6-F31   TO   UKE-F11
                   WRITE     UKE-REC
                   ADD       1         TO   CNT-UKEBWKF
              END-IF
*
              PERFORM   RUI6-READ-SEC
         ELSE
              MOVE     "END"     TO   RUISEK6-FLG
         END-IF
     END-PERFORM.
*
 UKEBWKF-WRITE1-EXIT.
     EXIT.
******************************************************************
*             実績累積ファイル読み込み（サカタ商品指定）
******************************************************************
 RUI-READ-SEC           SECTION.
*
     READ     RUISEKL3  NEXT
         AT END
              MOVE     "END"      TO   RUISEKF-FLG
         NOT AT END
              IF   RUI-F02  =  "30" OR "31" OR "32" OR "40" OR
                               "41" OR "50" OR "51" OR "70" OR
                               "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   RUI-READ-SEC
              END-IF
     END-READ.
*
 RUI-READ-EXIT.
     EXIT.
******************************************************************
*             実績累積ファイル読み込み（相手商品指定）
******************************************************************
 RUI6-READ-SEC           SECTION.
*
     READ     RUISEKL6  NEXT
         AT END
              MOVE     "END"      TO   RUISEK6-FLG
         NOT AT END
              IF   RUI6-F02  =  "30" OR "31" OR "32" OR "40" OR
                                "41" OR "50" OR "51" OR "70" OR
                                "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   RUI6-READ-SEC
              END-IF
     END-READ.
*
 RUI6-READ-EXIT.
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
     WHEN    "F005"
              MOVE  9             TO   END-FLG
                                  GO   TO   BODY-EXIT
     WHEN    "F004"
              MOVE      SPACE     TO   FJS05001
              MOVE     "SCREEN"   TO   DSP-GRP
              PERFORM   EDIT-SET-HEAD
              PERFORM   DSP-WR-SEC
              MOVE     "SYKKBN"   TO   SYORI-GROUP
              GO                  TO   BODY-EXIT
     WHEN    "F006"
              IF  SYKKBN = "1"
                  MOVE     "HEAD"     TO   SYORI-GROUP
              ELSE
                  MOVE     "HEAD1"    TO   SYORI-GROUP
              END-IF
              GO                      TO   BODY-EXIT
     WHEN    "E000"
              CONTINUE
     WHEN    "F011"
*             IF  SYKKBN = "1"
                  PERFORM   BEFORE-PAGE-SEC
*             ELSE
*                 PERFORM   BEFORE6-PAGE-SEC
*             END-IF
     WHEN    "F012"
*             IF  SYKKBN = "1"
                  PERFORM   NEXT-PAGE-SEC
*             ELSE
*                 PERFORM   NEXT6-PAGE-SEC
*             END-IF
     WHEN     OTHER
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
*             前レコード処理（サカタ商品指定）
******************************************************************
 BEFORE-REC-SEC         SECTION.
*
     MOVE     SPACE          TO   RUI-REC-INV.
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
     MOVE     SHOCD          TO   RUI-F10.
     MOVE     HIN1           TO   WK-HIN1.
     MOVE     HIN2           TO   WK-HIN2.
     MOVE     HIN3           TO   WK-HIN3.
     MOVE     WK-HINT        TO   RUI-F11.
     MOVE     SOKCD          TO   RUI-F17.
     MOVE     HIZUKE         TO   RUI-F09.
     START    RUISEKL3       KEY  <    RUI-F10
                                       RUI-F11
                                       RUI-F17
                                       RUI-F09
                 WITH   REVERSED  ORDER
         INVALID KEY
              MOVE      "INV"     TO   RUI-REC-INV
         NOT INVALID KEY
              PERFORM   RUI-NEXTREAD-SEC
     END-START.
     IF  RUI-REC-INV   =   "INV"
         MOVE      8    TO   ERR-NO
         MOVE     "C"   TO   EDIT-CURSOR  OF  SHOCD
         MOVE     "R"   TO   EDIT-OPTION  OF  SHOCD
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN1
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN2
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN3
         MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         PERFORM   ERR-DSP-SEC
         GO   TO   BEFORE-REC-EXIT
     END-IF.
     MOVE     RUI-F10        TO   SHOCD.
     MOVE     RUI-F11(1:5)   TO   HIN1.
     MOVE     RUI-F11(6:2)   TO   HIN2.
     MOVE     RUI-F11(8:1)   TO   HIN3.
*    商品名称マスタ検索
     MOVE     SPACE     TO   MEI-REC.
     INITIALIZE              MEI-REC.
     MOVE     SHOCD     TO   MEI-F011.
     MOVE     HIN1      TO   MEI-F0121.
     MOVE     HIN2      TO   MEI-F0122.
     MOVE     HIN3      TO   MEI-F0123.
     IF       SHOCD     NOT =     SPACE
              PERFORM   MEIMS-READ-SEC
     END-IF.
*    サブ商品名称マスタ検索
     MOVE     SPACE     TO   SUB-REC.
     INITIALIZE              SUB-REC.
     MOVE     SHOCD     TO   SUB-F011.
     MOVE     HIN1      TO   SUB-F0121.
     MOVE     HIN2      TO   SUB-F0122.
     MOVE     HIN3      TO   SUB-F0123.
     IF       SHOCD     NOT =     SPACE
              PERFORM   SUBMEIF-READ-SEC
     END-IF.
*    倉庫マスタ検索
     MOVE     RUI-F17        TO   SOKCD.
     IF       SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        MOVE      SPACE     TO   SOKNM
                   NOT INVALID
                        MOVE      SOK-F02   TO   SOKNM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKNM
     END-IF.
     MOVE     ZERO           TO   HIZUKE.
*
     MOVE    "SCREEN"        TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
*
 BEFORE-REC-EXIT.
     EXIT.
******************************************************************
*             前レコード処理（相手商品指定）
******************************************************************
 BEFORE6-REC-SEC         SECTION.
*
     MOVE     SPACE          TO   RUI6-REC-INV.
     MOVE     SPACE          TO   RUI6-REC.
     INITIALIZE                   RUI6-REC.
     MOVE     AITECD         TO   RUI6-F23.
     MOVE     SOKCD          TO   RUI6-F17.
     MOVE     HIZUKE         TO   RUI6-F09.
     START    RUISEKL6       KEY  <    RUI6-F23
                                       RUI6-F17
                                       RUI6-F09
                 WITH   REVERSED  ORDER
         INVALID KEY
              MOVE      "INV"     TO   RUI6-REC-INV
         NOT INVALID KEY
              PERFORM   RUI6-NEXTREAD-SEC
     END-START.
     IF  RUI6-REC-INV   =   "INV"
         MOVE      8    TO   ERR-NO
         MOVE     "C"   TO   EDIT-CURSOR  OF  AITECD
         MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
         MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         PERFORM   ERR-DSP-SEC
         GO   TO   BEFORE6-REC-EXIT
     END-IF.
*元↓MOVE     RUI6-F10        TO   SHOCD.
*    MOVE     RUI6-F11(1:5)   TO   HIN1.
*    MOVE     RUI6-F11(6:2)   TO   HIN2.
*    MOVE     RUI6-F11(8:1)   TO   HIN3.
*    商品名称マスタ検索
*    MOVE     SPACE     TO   MEI-REC.
*    INITIALIZE              MEI-REC.
*    MOVE     SHOCD     TO   MEI-F011.
*    MOVE     HIN1      TO   MEI-F0121.
*    MOVE     HIN2      TO   MEI-F0122.
*    MOVE     HIN3      TO   MEI-F0123.
*    IF       SHOCD     NOT =     SPACE
*             PERFORM   MEIMS-READ-SEC
*元↑END-IF.
*    商品変換テーブル検索
     MOVE     RUI6-F23  TO   AITECD.
     MOVE     SPACE     TO   SHO-REC.
     INITIALIZE              SHO-REC.
     MOVE     AITECD    TO   MEI-F02.
     IF       AITECD    NOT =     SPACE
              PERFORM   SHOTBL-READ-SEC
*元に従い     IF   SHOTBL-FLG      =   "INV"
*  エラーにしない  IF   ERR-NO    =    ZERO
*                       MOVE      18   TO  ERR-NO
*                       MOVE     "C"   TO  EDIT-CURSOR  OF  AITECD
*                  END-IF
*                  MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
*             END-IF
     END-IF.
*    サブ商品名称マスタ検索
     IF  SHOTBL-FLG      =   "   "
         MOVE     SPACE     TO   SUB-REC
         INITIALIZE              SUB-REC
         MOVE     SHO-F031  TO   SUB-F011
         MOVE     SHO-F0321 TO   SUB-F0121
         MOVE     SHO-F0322 TO   SUB-F0122
         MOVE     SHO-F0323 TO   SUB-F0123
         IF       SHO-F031  NOT =     SPACE
                  PERFORM   SUBMEIF-READ-SEC
*元に従い         IF   SUBMEI-FLG     =   "INV"
*  エラーにしない      IF   ERR-NO    =    ZERO
*                           MOVE   17  TO  ERR-NO
*                           MOVE  "C"  TO  EDIT-CURSOR  OF  AITECD
*                      END-IF
*                      MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
*                 END-IF
*        END-IF
     END-IF.
*    倉庫マスタ検索
     MOVE     RUI6-F17        TO   SOKCD.
     IF       SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        MOVE      SPACE     TO   SOKNM
                   NOT INVALID
                        MOVE      SOK-F02   TO   SOKNM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKNM
     END-IF.
     MOVE     ZERO           TO   HIZUKE.
*
     MOVE    "SCREEN"        TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
*
 BEFORE6-REC-EXIT.
     EXIT.
******************************************************************
*             次レコード処理（サカタ商品指定）
******************************************************************
 NEXT-REC-SEC           SECTION.
*
     MOVE     SPACE          TO   RUI-REC-INV.
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
     MOVE     SHOCD          TO   RUI-F10.
     MOVE     HIN1           TO   WK-HIN1.
     MOVE     HIN2           TO   WK-HIN2.
     MOVE     HIN3           TO   WK-HIN3.
     MOVE     WK-HINT        TO   RUI-F11.
     MOVE     SOKCD          TO   RUI-F17.
     MOVE     99999999       TO   RUI-F09.
     START    RUISEKL3       KEY  >    RUI-F10
                                       RUI-F11
                                       RUI-F17
                                       RUI-F09
         INVALID KEY
              MOVE      "INV"     TO   RUI-REC-INV
         NOT INVALID KEY
              PERFORM   RUI-NEXTREAD-SEC
     END-START.
     IF  RUI-REC-INV   =   "INV"
         MOVE      9    TO   ERR-NO
         MOVE     "C"   TO   EDIT-CURSOR  OF  SHOCD
         MOVE     "R"   TO   EDIT-OPTION  OF  SHOCD
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN1
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN2
         MOVE     "R"   TO   EDIT-OPTION  OF  HIN3
         MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         PERFORM   ERR-DSP-SEC
         GO   TO   NEXT-REC-EXIT
     END-IF.
     MOVE     RUI-F10        TO   SHOCD.
     MOVE     RUI-F11(1:5)   TO   HIN1.
     MOVE     RUI-F11(6:2)   TO   HIN2.
     MOVE     RUI-F11(8:1)   TO   HIN3.
*    商品名称マスタ検索
     MOVE     SPACE     TO   MEI-REC.
     INITIALIZE              MEI-REC.
     MOVE     SHOCD     TO   MEI-F011.
     MOVE     HIN1      TO   MEI-F0121.
     MOVE     HIN2      TO   MEI-F0122.
     MOVE     HIN3      TO   MEI-F0123.
     IF       SHOCD     NOT =     SPACE
              PERFORM   MEIMS-READ-SEC
     END-IF.
     MOVE     RUI-F17        TO   SOKCD.
     IF       SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        MOVE      SPACE     TO   SOKNM
                   NOT INVALID
                        MOVE      SOK-F02   TO   SOKNM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKNM
     END-IF.
     MOVE     ZERO           TO   HIZUKE.
*
     MOVE    "SCREEN"        TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
*
 NEXT-REC-EXIT.
     EXIT.
******************************************************************
*             次レコード処理（相手商品指定）
******************************************************************
 NEXT6-REC-SEC           SECTION.
*
     MOVE     SPACE          TO   RUI6-REC-INV.
     MOVE     SPACE          TO   RUI6-REC.
     INITIALIZE                   RUI6-REC.
     MOVE     AITECD         TO   RUI6-F23.
     MOVE     SOKCD          TO   RUI6-F17.
     MOVE     99999999       TO   RUI6-F09.
     START    RUISEKL6       KEY  >    RUI6-F23
                                       RUI6-F17
                                       RUI6-F09
         INVALID KEY
              MOVE      "INV"     TO   RUI6-REC-INV
         NOT INVALID KEY
              PERFORM   RUI6-NEXTREAD-SEC
     END-START.
     IF  RUI6-REC-INV   =   "INV"
         MOVE      9    TO   ERR-NO
         MOVE     "C"   TO   EDIT-CURSOR  OF  AITECD
         MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
         MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         PERFORM   ERR-DSP-SEC
         GO   TO   NEXT6-REC-EXIT
     END-IF.
*元  MOVE     RUI6-F10        TO   SHOCD.
*    MOVE     RUI6-F11(1:5)   TO   HIN1.
*    MOVE     RUI6-F11(6:2)   TO   HIN2.
*    MOVE     RUI6-F11(8:1)   TO   HIN3.
*元  商品名称マスタ検索
*    MOVE     SPACE     TO   MEI-REC.
*    INITIALIZE              MEI-REC.
*    MOVE     SHOCD     TO   MEI-F011.
*    MOVE     HIN1      TO   MEI-F0121.
*    MOVE     HIN2      TO   MEI-F0122.
*    MOVE     HIN3      TO   MEI-F0123.
*    IF       SHOCD     NOT =     SPACE
*             PERFORM   MEIMS-READ-SEC
*    END-IF.
*    商品変換テーブル検索
     MOVE     RUI6-F23  TO   AITECD.
     MOVE     SPACE     TO   SHO-REC.
     INITIALIZE              SHO-REC.
     MOVE     AITECD    TO   MEI-F02.
     IF       AITECD    NOT =     SPACE
              PERFORM   SHOTBL-READ-SEC
*元に従い     IF   SHOTBL-FLG      =   "INV"
*  エラーにしない  IF   ERR-NO    =    ZERO
*                       MOVE      18   TO  ERR-NO
*                       MOVE     "C"   TO  EDIT-CURSOR  OF  AITECD
*                  END-IF
*                  MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
*             END-IF
     END-IF.
*    サブ商品名称マスタ検索
     IF  SHOTBL-FLG      =   "   "
         MOVE     SPACE     TO   SUB-REC
         INITIALIZE              SUB-REC
         MOVE     SHO-F031  TO   SUB-F011
         MOVE     SHO-F0321 TO   SUB-F0121
         MOVE     SHO-F0322 TO   SUB-F0122
         MOVE     SHO-F0323 TO   SUB-F0123
         IF       SHO-F031  NOT =     SPACE
                  PERFORM   SUBMEIF-READ-SEC
*元に従い         IF   SUBMEI-FLG     =   "INV"
*  エラーにしない      IF   ERR-NO    =    ZERO
*                           MOVE   17  TO  ERR-NO
*                           MOVE  "C"  TO  EDIT-CURSOR  OF  AITECD
*                      END-IF
*                      MOVE     "R"   TO   EDIT-OPTION  OF  AITECD
*                 END-IF
*        END-IF
     END-IF.
*    倉庫マスタ検索
     MOVE     RUI6-F17        TO   SOKCD.
     IF       SOKCD     NOT  =    SPACE
              MOVE      SOKCD     TO   SOK-F01
              READ      ZSOKMS
                   INVALID
                        MOVE      SPACE     TO   SOKNM
                   NOT INVALID
                        MOVE      SOK-F02   TO   SOKNM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKNM
     END-IF.
     MOVE     ZERO           TO   HIZUKE.
*
     MOVE    "SCREEN"        TO   DSP-GRP.
     PERFORM            DSP-WR-SEC.
     PERFORM            ERR-DSP-SEC.
*
 NEXT6-REC-EXIT.
     EXIT.
******************************************************************
*             実績累積ファイル読み込み（サカタ商品指定）
******************************************************************
 RUI-NEXTREAD-SEC       SECTION.
*
     READ     RUISEKL3  NEXT
         AT END
              MOVE     "INV"      TO   RUI-REC-INV
         NOT AT END
              IF   RUI-F02  =  "30" OR "31" OR "32" OR "40" OR
                               "41" OR "50" OR "51" OR "70" OR
                               "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   RUI-NEXTREAD-SEC
              END-IF
     END-READ.
*
 RUI-NEXTREAD-EXIT.
     EXIT.
******************************************************************
*             実績累積ファイル読み込み（相手商品指定）
******************************************************************
 RUI6-NEXTREAD-SEC       SECTION.
*
     READ     RUISEKL6  NEXT
         AT END
              MOVE     "INV"      TO   RUI6-REC-INV
         NOT AT END
              IF   RUI6-F02  =  "30" OR "31" OR "32" OR "40" OR
                               "41" OR "50" OR "51" OR "70" OR
                               "71" OR "60" OR "42"
                   CONTINUE
              ELSE
                   GO   TO   RUI6-NEXTREAD-SEC
              END-IF
     END-READ.
*
 RUI6-NEXTREAD-EXIT.
     EXIT.
******************************************************************
*             前頁処理
******************************************************************
 BEFORE-PAGE-SEC        SECTION.
*
     IF       CNT-PAGE      >     1
              COMPUTE   CNT-PAGE  =  CNT-PAGE  - 1
              PERFORM   BODY-SET-SEC
     ELSE
              MOVE      6   TO    ERR-NO
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
*
 BEFORE-PAGE-EXIT.
     EXIT.
******************************************************************
*             次頁処理
******************************************************************
 NEXT-PAGE-SEC          SECTION.
*
     IF       CNT-PAGE      <     MAX-PAGE
              ADD       1   TO    CNT-PAGE
              PERFORM   BODY-SET-SEC
     ELSE
              MOVE      7   TO    ERR-NO
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
*
 NEXT-PAGE-EXIT.
     EXIT.
******************************************************************
*             商品名称マスタ読込み
******************************************************************
 MEIMS-READ-SEC         SECTION.
     MOVE     SPACE               TO   MEIMS-FLG.
     READ     HMEIMS
         INVALID
              MOVE  SPACE         TO   SHONM
              MOVE  "INV"         TO   MEIMS-FLG
         NOT  INVALID
              MOVE  MEI-F02       TO   WK-MEI-F02G
              MOVE  WK-MEI-F02    TO   SHONM
     END-READ.
 MEIMS-READ-EXIT.
     EXIT.
******************************************************************
*             商品変換テーブル読込み
******************************************************************
 SHOTBL-READ-SEC        SECTION.
     MOVE     SPACE               TO   SHOTBL-FLG.
     READ     SHOTBL7
         INVALID
              MOVE  "INV"         TO   SHOTBL-FLG
         NOT  INVALID
              MOVE  SPACE         TO   SHOTBL-FLG
     END-READ.
 SHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ読込
****************************************************************
 SUBMEIF-READ-SEC        SECTION.
*
     READ  SUBMEIL1
           INVALID      MOVE "INV"  TO SUBMEI-FLG
           NOT INVALID  MOVE  SPACE TO SUBMEI-FLG
                        MOVE  SUB-F02       TO   WK-MEI-F02G
                        MOVE  WK-MEI-F02    TO   SHONM
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
******************************************************************
*             ボディ部　セット処理
******************************************************************
 BODY-SET-SEC           SECTION.
*
     OPEN     INPUT     NUKEBWK1.
     IF       CNT-PAGE  >    MAX-PAGE
              GO   TO   BODY-SET-090
     END-IF.
*
     MOVE     SPACE     TO   BODYG.
     COMPUTE  WK-SEQ    =    CNT-PAGE  *   14   -   13.
     MOVE     WK-SEQ              TO   UKE-F01.
     START    NUKEBWK1  KEY  >=   UKE-F01
         INVALID
              GO   TO   BODY-SET-090
     END-START.
     READ     NUKEBWK1  NEXT
         AT END
              GO   TO   BODY-SET-090
     END-READ.
     MOVE     SPACE     TO   UKEBWKF-FLG.
     PERFORM  VARYING   IX1  FROM  1   BY   1
              UNTIL     IX1            >    14   OR
                        UKEBWKF-FLG    =   "END"
**************DISPLAY "IX1 = " IX1  UPON CONS
*        受払日
         MOVE      UKE-F02(3:6)   TO   SYUYMD(IX1)
*        入出庫事由
         IF   UKE-F04   =   "40" OR "41" OR "50" OR "51" OR
                            "70" OR "71" OR "42"
              MOVE     SPACE      TO   JYOKEN-REC
              INITIALIZE               JYOKEN-REC
              MOVE     1          TO   JYOKEN-F01
              MOVE     UKE-F04    TO   JYOKEN-F02
              READ     HJYOKEN
                   INVALID KEY
                       MOVE       ALL NC"＊"     TO   JIYU(IX1)
                   NOT INVALID KEY
                       MOVE       JYOKEN-F03     TO   JIYU(IX1)
              END-READ
         ELSE
              MOVE     SPACE      TO   KBM-REC
              INITIALIZE               KBM-REC
              MOVE     UKE-F07    TO   KBM-F01
              READ     SGYKBMF
                   INVALID KEY
                       MOVE       ALL NC"＊"     TO   JIYU(IX1)
                   NOT INVALID KEY
                       MOVE       KBM-F02        TO   JIYU(IX1)
              END-READ
         END-IF
*        伝票NO
         MOVE      UKE-F03        TO   DENNO (IX1)
*        D365伝票NO
         MOVE      UKE-F11        TO   D365NO (IX1)
*        入出
         IF        UKE-F05   =   "1"
                   MOVE      NC"入　"  TO   IRIDE(IX1)
         ELSE
              IF   UKE-F05   =   "2"
                   MOVE      NC"　出"    TO   IRIDE(IX1)
              ELSE
                   MOVE      NC"＊＊"    TO   IRIDE(IX1)
              END-IF
         END-IF
*        受払数量
         IF   UKE-F08   =   1  OR  3  OR  5  OR  7  OR  9
              COMPUTE   UKESUU(IX1)    =    UKE-F06  *  -1
         ELSE
              MOVE      UKE-F06        TO   UKESUU(IX1)
         END-IF
*        得意先名取得
*        IF   UKE-F04   =  "40" OR "41" OR "42"
*             MOVE      SPACE          TO   TOK-REC
*             INITIALIZE                    TOK-REC
*             MOVE      UKE-F09        TO   TOK-F52
*             READ      TOKMS3
*                       INVALID KEY
*                          MOVE   ALL NC"＊"   TO  TOKNM(IX1)
*                       NOT INVALID KEY
*                          MOVE   TOK-F03(1:8) TO  TOKNM(IX1)
*            END-READ
*        END-IF
*        得意先ＣＤ
         MOVE      UKE-F09        TO   MTOKCD(IX1)
*        担当者ＣＤ
         MOVE      UKE-F10        TO   TANCD (IX1)
*
         READ      NUKEBWK1  NEXT
              AT END
                   MOVE     "END"      TO   UKEBWKF-FLG
**************DISPLAY "IX2 = " IX1  UPON CONS
**************NOT  AT  END
**************DISPLAY "UKE-F06 = " UKE-F06  UPON CONS
         END-READ
*-------------------------
     END-PERFORM.
*
 BODY-SET-090.
*
     CLOSE    NUKEBWK1.
*
 BODY-SET-EXIT.
     EXIT.
************************************************************
*                項目制御部初期化（区分）　　              *
************************************************************
 EDIT-SET-KBN           SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SYKKBN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SYKKBN.
 EDIT-SET-KBN-END.
     EXIT.
************************************************************
*                項目制御部初期化（ヘッド部）              *
************************************************************
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SHOCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  HIN1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HIN1.
     MOVE   "M"     TO   EDIT-OPTION  OF  HIN2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HIN2.
     MOVE   "M"     TO   EDIT-OPTION  OF  HIN3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HIN3.
     MOVE   "M"     TO   EDIT-OPTION  OF  AITECD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  AITECD.
     MOVE   "M"     TO   EDIT-OPTION  OF  SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  HIZUKE.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HIZUKE.
 EDIT-SET-HEAD-END.
     EXIT.
****************************************************************
*             ＰＦキーガイダンス
****************************************************************
 PFK-DSP-SEC            SECTION.
     IF       SYORI-GROUP         =   "HEAD" OR "HEAD1"
         MOVE  PMSG01             TO   MO002
     ELSE
         MOVE  PMSG02             TO   MO002
     END-IF.
     IF       SYORI-GROUP         =   "SYKKBN"
         MOVE  PMSG00             TO   MO002
     END-IF.
     MOVE     "MO002"             TO   DSP-GRP.
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
     CLOSE         RUISEKL3
                   RUISEKL6
                   HMEIMS
                   SUBMEIL1
                   SHOTBL7
                   HJYOKEN
                   ZSOKMS
                   SGYKBMF
                   TOKMS3
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
     MOVE     WRK-PGID            TO   PGID.
     MOVE     WRK-FORMID          TO   FORMID.
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
*    MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     WRITE    FJS05001.
 DSP-WR-EXIT.
     EXIT.

```
