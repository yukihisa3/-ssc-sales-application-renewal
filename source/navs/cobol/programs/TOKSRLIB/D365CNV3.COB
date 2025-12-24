****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品変換ＴＢＬコンバート　　　　　　　  *
*    作成日／作成者：　2021/06/02 INOUE                        *
*    処理概要　　　：　基幹システムマスタより該当データ　      *
*                      を編集・抽出する　　　　　　　　　      *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            D365CNV3.
*                  流用:D365CNV2
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 商品変換ＴＢＬ　 >>----*
     SELECT   SHOTBL1   ASSIGN              DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE      STATUS    TBL-STATUS.
*----<< SUB商品名称マスタ　 >>----*
     SELECT   SUBMEIL1  ASSIGN              DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*----<< 移行商品データ　 >>----*
     SELECT   IMPSHOD1  ASSIGN              DA-01-VI-IMPSHOD1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       IN-F01
                                            IN-F02
                        FILE      STATUS    IN-STATUS.
*----<< 商品変換ＴＢＬ　作成データ >>----*
     SELECT   CRTTBLF   ASSIGN    TO        DA-01-VS-CRTTBLF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CRT-STATUS.
*----<< SUB商品変換ＴＢＬ　作成データ >>----*
     SELECT   CRTSTBLF  ASSIGN    TO        DA-01-VS-CRTSTBLF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CRS-STATUS.
*----<< SUB名称マスタ非存在データ >>----*
     SELECT   NOSMEIF   ASSIGN    TO        DA-01-VS-NOSMEIF
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
 FD  SHOTBL1   LABEL RECORD   IS   STANDARD.
     COPY      SHOTBL1        OF   XFDLIB
     JOINING   TBL                 PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1  LABEL RECORD   IS   STANDARD.
     COPY      SUBMEIL1         OF   XFDLIB
     JOINING   MEI                 PREFIX.
******************************************************************
*    商品変換ＴＢＬ　作成データ
******************************************************************
 FD  CRTTBLF  LABEL RECORD   IS   STANDARD.
     COPY     CRTTBLF        OF   XFDLIB
     JOINING  CRT                 PREFIX.
******************************************************************
*    SUB商品変換ＴＢＬ　作成データ
******************************************************************
 FD  CRTSTBLF  LABEL RECORD   IS   STANDARD.
     COPY      CRTSTBLF       OF   XFDLIB
     JOINING   CRS                 PREFIX.
******************************************************************
*    名称マスタ非存在データ
******************************************************************
 FD  NOSMEIF   LABEL RECORD   IS   STANDARD.
     COPY      NOSMEIF         OF   XFDLIB
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
     03  CRTTBLF-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  IMPSHOD1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHOTBL1-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SUBMEIL1-INV-FLG      PIC  X(03)     VALUE  SPACE.
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
     03  CRT-STATUS        PIC  X(02).
     03  CRS-STATUS        PIC  X(02).
     03  NON-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "D365CNV3".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV3".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV3".
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
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL1.
     MOVE      "SUBMEIL1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CRTTBLF.
     MOVE      "CRTTBLF"    TO   AB-FILE.
     MOVE      CRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CRTSTBLF.
     MOVE      "CRTSTBLF"   TO   AB-FILE.
     MOVE      CRS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NOSMEIF.
     MOVE      "NOSMEIF  "   TO   AB-FILE.
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
     OPEN     INPUT     SHOTBL1   IMPSHOD1  SUBMEIL1.
     OPEN     OUTPUT    CRTTBLF   CRTSTBLF   NOSMEIF.
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
*    商品変換ＴＢＬ読込み
     PERFORM SHOTBL1-READ-SEC.
     DISPLAY "-------------------------------------" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SHOTBL1-READ-SEC    SECTION.
*
     MOVE    "SHOTBL1-READ-SEC"     TO  S-NAME.
*
     READ     SHOTBL1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO SHOTBL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN001.
*    SUB商品名称マスタ検索
     MOVE     TBL-F031       TO  MEI-F011.
     MOVE     TBL-F0321      TO  MEI-F0121.
     MOVE     TBL-F0322      TO  MEI-F0122.
     MOVE     TBL-F0323      TO  MEI-F0123.
     READ     SUBMEIL1
         INVALID
              DISPLAY "SUB" NC"名称" NC"なし" "] "
                      NC"商" "=" TBL-F031 " "
                      NC"単" "=" TBL-F032 UPON CONS
              PERFORM 3-NON-MEI-SEC
              ADD   1        TO  SKIP2-CNT
              GO             TO  MAIN020
         NOT INVALID
              PERFORM 1-CRT-TBL-SEC
              PERFORM 2-CRT-STBL-SEC
              ADD   1        TO  WRT-CNT
              GO             TO  MAIN020
     END-READ.
*
 MAIN020.
*    移行商品データ　読込み
     PERFORM SHOTBL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　_変換ＴＢＬ作成データ出力（存在時）　　　　　　　*
****************************************************************
 1-CRT-TBL-SEC   SECTION.
*
     MOVE    "1-CRT-TBL-SEC"         TO   S-NAME.
*
 1-CRT-TBL-001.
*
*  レコード転送
     INITIALIZE                  CRT-REC.
     MOVE     TBL-REC        TO  CRT-REC.
*
* 取引先コード
* 量販店商品コード
* 商品コード
* 品単１
* 品単２
* 品単３
* 出荷場所
* 原価単価
* 売価単価
* 分類コード
* _番
* 仕入単価
* ﾗﾍﾞﾙ張替区分
* 検品Ｇ
* 出荷地域区分
* 登録担当者
* 最終更新担当者
* 登録日
     MOVE     SYS-DATEW      TO  CRT-F98.
* 更新日
     MOVE     SYS-DATEW      TO  CRT-F99.
*
 1-CRT-TBL-002.
*
     WRITE    CRT-REC.
*
 1-CRT-TBL-EXIT.
     EXIT.
****************************************************************
*　　　_ＳＵＢ変換ＴＢＬ作成データ出力（共通）　　　　　　　　*
****************************************************************
 2-CRT-STBL-SEC   SECTION.
*
     MOVE    "2-CRT-STBL-SEC"         TO   S-NAME.
*
 2-CRT-STBL-001.
*
*  移行商品データ検索
*  　※先行「名称マスタコンバート」仕様上
*  　　ここでのINVALIDは発生しない前提
     MOVE     TBL-F031       TO  IN-F01.
     MOVE     TBL-F032       TO  IN-F02.
     READ     IMPSHOD1
         INVALID
              INITIALIZE         IN-REC
*             DISPLAY NC"移行商品データなし！　商品＝"
*                                          TBL-F31  UPON CONS
*             DISPLAY NC"　　　　　　　　　　　品単＝"
*                                          TBL-F032 UPON CONS
*  レコード転送
     INITIALIZE                  CRS-REC.
     MOVE     CRT-REC        TO  CRS-REC.
*  取引先コード
*  量販店商品コード
*  自社商品コード
*  品単１
*  品単２
*  品単３
*  出荷場所
*  原価単価
*  売価単価
*  分類コード
*  _番
*  仕入単価
*  ﾗﾍﾞﾙ張替区分
*  検品Ｇ
*  出荷地域区分
*  予備JANCD
     MOVE     IN-F05         TO  CRS-F13.
*  予備インストアCD
     MOVE     SPACE          TO  CRS-F14.
*  入数
     MOVE     ZERO           TO  CRS-F15.
*  振分倉庫CD
     MOVE     SPACE          TO  CRS-F16.
*  D365　JANCD
     MOVE     IN-F05         TO  CRS-F17.
*  D365　商品CD
     MOVE     IN-F06         TO  CRS-F18.
*  直送／セット組区分
     MOVE     SPACE          TO  CRS-F19.
*  予備領域
     MOVE     SPACE          TO  CRS-FIL.
*  登録担当者部門CD
     MOVE     "2920"         TO  CRS-F92.
*  登録担当者CD
     MOVE     "99"           TO  CRS-F93.
*  登録日付
     MOVE     SYS-DATEW      TO  CRS-F94.
*  登録時刻
     MOVE     WK-TIME(1:6)   TO  CRS-F95.
*  更新担当者部門CD
     MOVE     "2920"         TO  CRS-F96.
*  更新担当者CD
     MOVE     "99"           TO  CRS-F97.
*  更新日付
     MOVE     SYS-DATEW      TO  CRS-F98.
*  更新時刻
     MOVE     WK-TIME(1:6)   TO  CRS-F99.
*
 2-CRT-STBL-002.
*
     WRITE    CRS-REC.
*
 2-CRT-STBL-EXIT.
     EXIT.
****************************************************************
*　　　　　　_ＳＵＢ名称マスタ非存在データ出力（非存在時）　　*
****************************************************************
 3-NON-MEI-SEC   SECTION.
*
     MOVE    "3-NON-MEI-SEC"         TO   S-NAME.
*
 3-NON-MEI-001.
*
*  レコード転送
     INITIALIZE                  NON-REC.
     MOVE     TBL-REC        TO  NON-REC.
*
 3-NON-MEI-002.
*
     WRITE    NON-REC.
*
 3-NON-MEI-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"商品変換ＴＢＬ" ":SHOTBL1  "         UPON CONS.
     DISPLAY NC"　　　　　　　" " IN  = " READ-CNT   UPON CONS.
     DISPLAY NC"ＳＵＢ名称なし" ":NOSMEIF   "        UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP2-CNT  UPON CONS.
     DISPLAY NC"出力データ　　" ":CRTTBLF  "         UPON CONS.
     DISPLAY NC"　　　　　　　" ":CRTSTBLF "         UPON CONS.
     DISPLAY NC"　　　　　　　" " OUT = " WRT-CNT    UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*
     CLOSE     SHOTBL1   IMPSHOD1  SUBMEIL1
               CRTTBLF   CRTSTBLF  NOSMEIF.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
