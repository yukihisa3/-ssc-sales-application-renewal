# D365CNV4

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/D365CNV4.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品名称マスタコンバート　　　　　　　  *
*    作成日／作成者：　2021/06/04 INOUE                        *
*    処理概要　　　：　作成データから名称マスタ　　　　　      *
*                      を作成・更新する　　　　　　　　　      *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            D365CNV4.
*                  流用:D365CNV2
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 商品名称マスタ　作成データ >>----*
     SELECT   CRTMEIF   ASSIGN    TO        DA-01-VS-CRTMEIF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CRM-STATUS.
*----<< SUB商品名称マスタ　作成データ >>----*
*    SELECT   CRTSMEIF  ASSIGN    TO        DA-01-VS-CRTSMEIF
*                       ORGANIZATION        SEQUENTIAL
*                       ACCESS    MODE      SEQUENTIAL
*                       FILE      STATUS    CRS-STATUS.
*----<< 商品名称マスタ　 >>----*
     SELECT   MEIMS1    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    商品変換ＴＢＬ　
******************************************************************
*FD  SHOTBL1   LABEL RECORD   IS   STANDARD.
*    COPY      SHOTBL1        OF   XFDLIB
*    JOINING   TBL                 PREFIX.
******************************************************************
*    商品名称マスタ　作成データ
******************************************************************
 FD  CRTMEIF  LABEL RECORD   IS   STANDARD.
     COPY      CRTMEIF        OF   XFDLIB
     JOINING   IN                  PREFIX.
******************************************************************
*    SUB商品名称マスタ　作成データ
******************************************************************
*FD  CRTSMEIF  LABEL RECORD   IS   STANDARD.
*    COPY      CRTSMEIF       OF   XFDLIB
*    JOINING   IN                  PREFIX.
******************************************************************
*    商品名称マスタ　
******************************************************************
 FD  MEIMS1    LABEL RECORD   IS   STANDARD.
     COPY      MEIMS1         OF   XFDLIB
     JOINING   MEI                 PREFIX.
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
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  CRTMEIF-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  IMPSHOL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
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
         05  ST-PG          PIC   X(08)  VALUE "D365CNV4".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV4".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV4".
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
*FILEERR-SEC5           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   CRTSMEIF.
*    MOVE      "CRTSMEIF"   TO   AB-FILE.
*    MOVE      CRS-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
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
     OPEN     INPUT     CRTMEIF.
*    OPEN     INPUT     CRTSMEIF.
     OPEN     I-O       MEIMS1.
*    OPEN     I-O       SUBMEIL1.
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
*    START    IMPSHOL1  KEY  >=   IN-F01
*        INVALID   KEY
*             MOVE    "END"  TO   END-FLG
*             GO             TO   INIT-EXIT
*    END-START.
*
*    作成データ読込み
     PERFORM IN-READ-SEC.
     DISPLAY "-------------------------------------" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IN-READ-SEC    SECTION.
*
     MOVE    "IN-READ-SEC"    TO  S-NAME.
*
     READ     CRTMEIF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  IN-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 IN-READ-EXIT.
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
     MOVE     IN-F011        TO  MEI-F011.
     MOVE     IN-F0121       TO  MEI-F0121.
     MOVE     IN-F0122       TO  MEI-F0122.
     MOVE     IN-F0123       TO  MEI-F0123.
     READ     MEIMS1
         INVALID
*TEST
              DISPLAY NC"作成　"
                    NC"商" "=" IN-F011 " " NC"単" "= " IN-F012
                      UPON CONS
*TEST
              MOVE    IN-REC     TO   MEI-REC
              WRITE   MEI-REC
              ADD   1        TO  WRT-CNT
              GO             TO  MAIN020
         NOT INVALID
*TEST
              DISPLAY NC"上書　"
                    NC"商" "=" IN-F011 " " NC"単" "= " IN-F012
                      UPON CONS
*TEST
              MOVE    IN-REC     TO   MEI-REC
              REWRITE MEI-REC
              ADD   1        TO  RWT-CNT
              GO             TO  MAIN020
     END-READ.
*
 MAIN020.
*    作成データ　読込み
     PERFORM IN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"作成対象データ" ":CRTMEIF  "         UPON CONS.
     DISPLAY NC"　　　　　　　" " IN  = " READ-CNT   UPON CONS.
     DISPLAY NC"名称マスタ作成" " OUT = " WRT-CNT    UPON CONS.
     DISPLAY NC"名称マスタ上書" " UPDT= " RWT-CNT    UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*
     CLOSE     MEIMS1  CRTMEIF.
*    CLOSE     SUBMEIL1  CRTSMEIF.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
