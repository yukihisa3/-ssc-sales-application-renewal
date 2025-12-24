****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品名称マスタコンバート　　　　　　　  *
*    作成日／作成者：　2021/06/01 INOUE                        *
*    処理概要　　　：　基幹システムマスタより該当データ　      *
*                      を編集・抽出する　　　　　　　　　      *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            D365CNV2.
*                  流用:FLMCNV03
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 移行商品データ　 >>----*
     SELECT   IMPSHOD1  ASSIGN              DA-01-VI-IMPSHOD1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IN-F01
                                            IN-F02
                        FILE      STATUS    IN-STATUS.
*----<< 商品変換ＴＢＬ　 >>----*
*    SELECT   SHOTBL1   ASSIGN              DA-01-VI-SHOTBL1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       TBL-F01
*                                           TBL-F02
*                       FILE      STATUS    TBL-STATUS.
*----<< 商品名称マスタ　 >>----*
     SELECT   MEIMS1    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*----<< 商品名称マスタ　作成データ >>----*
     SELECT   CRTMEIF   ASSIGN    TO        DA-01-VS-CRTMEIF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CRM-STATUS.
*----<< SUB商品名称マスタ　作成データ >>----*
     SELECT   CRTSMEIF  ASSIGN    TO        DA-01-VS-CRTSMEIF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CRS-STATUS.
*----<< 名称マスタ非存在データ >>----*
     SELECT   NOMEIF    ASSIGN    TO        DA-01-VS-NOMEIF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    NON-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    移行商品データ　
******************************************************************
 FD  IMPSHOD1  LABEL RECORD   IS   STANDARD.
     COPY      IMPSHOD1       OF   XFDLIB
     JOINING   IN                  PREFIX.
******************************************************************
*    商品変換ＴＢＬ　
******************************************************************
*FD  SHOTBL1   LABEL RECORD   IS   STANDARD.
*    COPY      SHOTBL1        OF   XFDLIB
*    JOINING   TBL                 PREFIX.
******************************************************************
*    商品名称マスタ　
******************************************************************
 FD  MEIMS1    LABEL RECORD   IS   STANDARD.
     COPY      MEIMS1         OF   XFDLIB
     JOINING   MEI                 PREFIX.
******************************************************************
*    商品名称マスタ　作成データ
******************************************************************
 FD  CRTMEIF  LABEL RECORD   IS   STANDARD.
     COPY      CRTMEIF        OF   XFDLIB
     JOINING   CRM                 PREFIX.
******************************************************************
*    SUB商品名称マスタ　作成データ
******************************************************************
 FD  CRTSMEIF  LABEL RECORD   IS   STANDARD.
     COPY      CRTSMEIF       OF   XFDLIB
     JOINING   CRS                 PREFIX.
******************************************************************
*    名称マスタ非存在データ
******************************************************************
 FD  NOMEIF    LABEL RECORD   IS   STANDARD.
     COPY      NOMEIF         OF   XFDLIB
     JOINING   NON                 PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  CRTMEIF-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  IMPSHOD1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHOTBL1-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*
 01  WK-ST.
     03  MEI-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CRM-STATUS        PIC  X(02).
     03  CRS-STATUS        PIC  X(02).
     03  NON-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "D365CNV2".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV2".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV2".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-JDATE             PIC   9(08).
*01  PARA-IN-JTIME             PIC   9(04).
*01  PARA-IN-TORICD            PIC   9(08).
*01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   IMPSHOD1.
     MOVE      "IMPSHOD1 "   TO   AB-FILE.
     MOVE      IN-STATUS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC2           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   SHOTBL1.
*    MOVE      "SHOTBL1 "   TO   AB-FILE.
*    MOVE      TBL-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CRTMEIF.
     MOVE      "CRTMEIF"   TO   AB-FILE.
     MOVE      CRM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CRTSMEIF.
     MOVE      "CRTSMEIF"   TO   AB-FILE.
     MOVE      CRS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NOMEIF.
     MOVE      "NOMEIF  "   TO   AB-FILE.
     MOVE      NON-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     IMPSHOD1  MEIMS1.
     OPEN     OUTPUT    CRTMEIF   CRTSMEIF   NOMEIF.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    移行商品データＳＴＡＲＴ
*    MOVE     SPACE          TO   IN-REC.
*    INITIALIZE                   IN-REC.
*    MOVE     137607         TO   IN-F01.
*    START    IMPSHOD1  KEY  >=   IN-F01
*        INVALID   KEY
*             MOVE    "END"  TO   END-FLG
*             GO             TO   INIT-EXIT
*    END-START.
*
*    移行商品データ読込み
     PERFORM IMPSHOD1-READ-SEC.
     DISPLAY "-------------------------------------" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IMPSHOD1-READ-SEC    SECTION.
*
     MOVE    "IMPSHOD1-READ-SEC"    TO  S-NAME.
*
     READ     IMPSHOD1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  IMPSHOD1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 IMPSHOD1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN001.
*    商品名称マスタ検索
     MOVE     IN-F01         TO  MEI-F011.
     MOVE     IN-F02(1:5)    TO  MEI-F0121.
     MOVE     IN-F02(6:2)    TO  MEI-F0122.
     MOVE     IN-F02(8:1)    TO  MEI-F0123.
     READ     MEIMS1
         INVALID
              DISPLAY NC"名称" "M" NC"なし" "]" " "
                      NC"商" "=" IN-F01 " " NC"単" "= " IN-F02
                      UPON CONS
*             DISPLAY NC"商品名称マスタなし！　商品＝" IN-F01
*                     " "  NC"品単＝"   IN-F02   UPON CONS
              PERFORM 3-CRT-MEI-SEC
              PERFORM 2-CRT-SMEI-SEC
              ADD   1        TO  WRT-CNT
              PERFORM 4-NON-MEI-SEC
              ADD   1        TO  SKIP2-CNT
              GO             TO  MAIN020
         NOT INVALID
              PERFORM 1-CRT-MEI-SEC
              PERFORM 2-CRT-SMEI-SEC
              ADD   1        TO  WRT-CNT
              GO             TO  MAIN020
     END-READ.
*
 MAIN020.
*    移行商品データ　読込み
     PERFORM IMPSHOD1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　_名称マスタ作成データ出力（存在時）　　　　　　　*
****************************************************************
 1-CRT-MEI-SEC   SECTION.
*
     MOVE    "1-CRT-MEI-SEC"         TO   S-NAME.
*
 1-CRT-MEI-001.
*
*  レコード転送
     INITIALIZE                  CRM-REC.
     MOVE     MEI-REC        TO  CRM-REC.
*  商品コード
     MOVE     IN-F03         TO  CRM-F011.
*  単１
     MOVE     IN-F04(1:5)    TO  CRM-F0121.
*  単２
     MOVE     IN-F04(6:2)    TO  CRM-F0122.
*  単３
     MOVE     IN-F04(8:1)    TO  CRM-F0123.
*  商品漢字名１
*  商品漢字名２
*  商品カナ名１
*  商品カナ名２
*  原価単価
*  売価単価
*  小売単価
*  主仕入先コード
*  ＪＡＮコード
     MOVE     IN-F05         TO  CRM-F06.
*  入数
*  廃盤区分
*  サカタ２０分類
     MOVE     IN-F11         TO  CRM-F09.
*  小売連携区分
*  予備領域１(振替商品区分)
*  物流束区分
*  商品分類区分
*  管理区分
*  自動発注区分
*  商品名称区分
*  ユポラベル区分
*  定番区分
*  季節区分
*  予備区分
*  登録日
     MOVE     SYS-DATEW      TO  CRM-F98.
*  更新日
     MOVE     SYS-DATEW      TO  CRM-F99.
*
 1-CRT-MEI-002.
*
     WRITE    CRM-REC.
*
 1-CRT-MEI-EXIT.
     EXIT.
****************************************************************
*　　　_ＳＵＢ名称マスタ作成データ出力（共通）　　　　　　　　*
****************************************************************
 2-CRT-SMEI-SEC   SECTION.
*
     MOVE    "2-CRT-SMEI-SEC"         TO   S-NAME.
*
 2-CRT-SMEI-001.
*
*  レコード転送
     INITIALIZE                  CRS-REC.
     MOVE     CRM-REC        TO  CRS-REC.
*  商品コード
*  単１
*  単２
*  単３
*  商品名１
*  商品名２
*  商品名カナ１
*  商品名カナ２
*  原価単価
*  卸単価
*  売価単価
*  仕入先ＣＤ
*  ＪＡＮＣＤ
*  入数
*  廃盤区分
*  サカタ２０分類
*  小売連携区分
*  振替商品区分
*  物流束区分
*  商品分類CD（営業２部用）
*  管理区分
*  自動発注区分
*  商品名称区分
*  ユポラベル区分
*  定番区分
*  季節区分
*  予備区分
*  登録日
*  更新日
*  Ｄ３６５用　JANCD
     MOVE     IN-F05         TO  CRS-D01.
*  Ｄ３６５用　商品CD
     MOVE     IN-F06         TO  CRS-D02.
*  オーダー区分
     MOVE     "D04"          TO  CRS-D03.
*  予備領域
*  登録担当者部門CD
     MOVE     "2920"         TO  CRS-D92.
*  登録担当者CD
     MOVE     "99"           TO  CRS-D93.
*  登録日付
     MOVE     CRM-F98        TO  CRS-D94.
*  登録時刻
     MOVE     WK-TIME(1:6)   TO  CRS-D95.
*  更新担当者部門CD
     MOVE     "2920"         TO  CRS-D96.
*  更新担当者CD
     MOVE     "99"           TO  CRS-D97.
*  更新日付
     MOVE     CRM-F98        TO  CRS-D98.
*  更新時刻
     MOVE     WK-TIME(1:6)   TO  CRS-D99.
*
 2-CRT-SMEI-002.
*
     WRITE    CRS-REC.
*
 2-CRT-SMEI-EXIT.
     EXIT.
****************************************************************
*　　　　　　_名称マスタ作成データ出力（非存在時）　　　　　　*
****************************************************************
 3-CRT-MEI-SEC   SECTION.
*
     MOVE    "3-CRT-MEI-SEC"         TO   S-NAME.
*
 3-CRT-MEI-001.
*
*  レコード転送
     INITIALIZE                  CRM-REC.
*  商品コード
     MOVE     IN-F03         TO  CRM-F011.
*  単１
     MOVE     IN-F04(1:5)    TO  CRM-F0121.
*  単２
     MOVE     IN-F04(6:2)    TO  CRM-F0122.
*  単３
     MOVE     IN-F04(8:1)    TO  CRM-F0123.
*  商品漢字名１
     MOVE     IN-F07         TO  CRM-F021.
*  商品漢字名２
     MOVE     IN-F08         TO  CRM-F022.
*  商品カナ名１
     MOVE     IN-F09         TO  CRM-F031.
*  商品カナ名２
     MOVE     IN-F10         TO  CRM-F032.
*  原価単価
*  売価単価
*  小売単価
*  主仕入先コード
     MOVE    "29299999"      TO  CRM-F05.
*  ＪＡＮコード
     MOVE     IN-F05         TO  CRM-F06.
*  入数
*  廃盤区分
*  サカタ２０分類
     MOVE     IN-F11         TO  CRM-F09.
*  小売連携区分
*  予備領域１(振替商品区分)
*  物流束区分
*  商品分類区分
     MOVE    "2"             TO  CRM-F90.
*  管理区分
*  自動発注区分
*  商品名称区分
*  ユポラベル区分
*  定番区分
*  季節区分
     MOVE    "1"             TO  CRM-F96.
*  予備区分
*  登録日
     MOVE     SYS-DATEW      TO  CRM-F98.
*  更新日
     MOVE     SYS-DATEW      TO  CRM-F99.
*
 3-CRT-MEI-002.
*
     WRITE    CRM-REC.
*
 3-CRT-MEI-EXIT.
     EXIT.
****************************************************************
*　　　　　　_名称マスタ非存在データ出力（非存在時）　　　　　*
****************************************************************
 4-NON-MEI-SEC   SECTION.
*
     MOVE    "4-NON-MEI-SEC"         TO   S-NAME.
*
 4-NON-MEI-001.
*
*  レコード転送
     INITIALIZE                  NON-REC.
     MOVE     IN-REC         TO  NON-REC.
*
 4-NON-MEI-002.
*
     WRITE    NON-REC.
*
 4-NON-MEI-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"移行商品データ" ":IMPSHOD1 "         UPON CONS.
     DISPLAY NC"　　　　　　　" " IN  = " READ-CNT   UPON CONS.
     DISPLAY NC"　名称Ｍなし　" ":NOMEIF   "         UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP2-CNT  UPON CONS.
     DISPLAY NC"出力データ　　" ":CRTMEIF  "         UPON CONS.
     DISPLAY NC"　　　　　　　" ":CRTSMEIF "         UPON CONS.
     DISPLAY NC"　　　　　　　" " OUT = " WRT-CNT    UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*
     CLOSE     IMPSHOD1  MEIMS1  CRTMEIF  CRTSMEIF  NOMEIF.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
