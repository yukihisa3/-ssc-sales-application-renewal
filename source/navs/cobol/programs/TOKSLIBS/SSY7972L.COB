****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ダイキセンター納品対応　　　　　　*
*    モジュール名　　　　：　個口数記入一覧表発行　　　　　　　*
*    作成日／更新日　　　：　2006/07/28                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキセンター納品データを読み、　*
*                        ：　個口数記入一覧表を出力する。　　　*
*                        ：　                                  *
*2007/11/19 DCMJAPANへの変更による発注種別区分修正             *
*2008/02/20 部門CD変更
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7972L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/07/28.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<センター納品データ　　　　　　>>----*
     SELECT   DSCENTF   ASSIGN    TO        DA-01-VI-DSCENTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F16   DKC-F07
                        FILE      STATUS    DKC-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE      STATUS    TEN-ST.
*----<< 倉庫マスタ >>--*
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        FILE      STATUS    PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<センター納品データ　　　　　　>>----*
 FD  DSCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DSCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
*----<<プリントファイル　　　　　　　>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  DKC-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
     03  PAGE-FLG       PIC  9(01)   VALUE ZERO.
     03  TENPO-FLG      PIC  9(01)   VALUE ZERO.
     03  NOUHIN-FLG     PIC  9(01)   VALUE ZERO.
 01  WK-CNT.
     03  DKC-CNT        PIC  9(07).
     03  TOK-CNT        PIC  9(07).
     03  TEN-CNT        PIC  9(07).
     03  PAGE-CNT       PIC  9(03).
     03  LINE-CNT       PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  DKC-ST         PIC  X(02).
     03  TOK-ST         PIC  X(02).
     03  TEN-ST         PIC  X(02).
     03  SOK-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  MAX-LINE          PIC  9(02)      VALUE  60.
 01  PG-ID             PIC  X(08)      VALUE  "SSY7972L".
 01  WK-MSG1           PIC  N(14)
                       VALUE NC"指定のデータはありません。".
*----<< ﾜｰｸ >>--*
 01  WK-AREA.
   03  WK-TENCD        PIC  9(05).
   03  WK-NOUHIN       PIC  9(08).
   03  WK-SOKCD        PIC  X(02).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(25)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"　※※　ダイキ　個口数記入一覧表　※※　"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(21)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD15.
     03  FILLER                  PIC  X(43)  VALUE  SPACE.
     03  HD15-KU0                PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD15-02YY               PIC  9999.
     03  HD15-KU1                PIC  X(01).
     03  HD15-02MM               PIC  99.
     03  HD15-KU2                PIC  X(01).
     03  HD15-02DD               PIC  99.
     03  HD15-KU5                PIC  X(01).
     03  HD15-03HH               PIC  99.
     03  HD15-KU3                PIC  X(01).
     03  HD15-03MM               PIC  99.
     03  HD15-KU6                PIC  X(01).
     03  HD15-04                 PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD15-KU4                PIC  X(01).
*
 01  HD2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"倉　庫"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  "ｺｰﾄﾞ:".
     03  HD2-01                  PIC  X(02).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"倉庫名"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  HD2-TOKNM               PIC  N(10)
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  HD3.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"納品日"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"店"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"店舗名"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"部門"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"部門名"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(16)  VALUE  SPACE.
*****03  FILLER                  PIC  N(08)  VALUE
*****                            NC"小物　　　　異形"
*****                            CHARACTER   TYPE  IS  CHR-2.
*****03  FILLER                  PIC  X(07)  VALUE  SPACE.
*****03  FILLER                  PIC  N(08)  VALUE
*****                            NC"ケース　　　　他"
*****                            CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(10)  VALUE
                                 NC"　小物　　　　　異形"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(10)  VALUE
                                 NC"　ケース　　　　　他"
                                 CHARACTER   TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "                                 -----------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-03                  PIC  ZZZZ.
     03  DT1-HIFN1               PIC  X.
     03  DT1-04                  PIC  ZZ.
     03  DT1-HIFN2               PIC  X.
     03  DT1-05                  PIC  ZZ.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-01A.
       05  DT1-01                PIC  9999.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(06).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-06                  PIC  999.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-07                  PIC  N(08).
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  X(55)  VALUE
     "(          )  (          )  (          )  (          )".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-08                  PIC  N(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
****************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 DKCERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSCENTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7972L DKC ERROR " DKC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*得意先マスタ
 TOKERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7972L TOK ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*店舗マスタ
 TENERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7972L TEN ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*倉庫マスタ
 SOKERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7972L SOK ERROR " SOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7972L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN
              UNTIL     END-FLG   =  "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7972L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DSCENTF   HTOKMS    TENMS1   ZSOKMS.
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*センター納品データスタート
     MOVE     SPACE          TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE     PARA-JDATE     TO   DKC-F01.
     MOVE     PARA-JTIME     TO   DKC-F02.
     MOVE     PARA-TORICD    TO   DKC-F03.
     MOVE     PARA-SOKO      TO   DKC-F04.
     START    DSCENTF   KEY  >=   DKC-F01   DKC-F02
                                  DKC-F03   DKC-F04
                                  DKC-F05   DKC-F06
                                  DKC-F16   DKC-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-START.
*    センター納品データ読込み
     PERFORM DSCENTF-READ-SEC.
     IF      END-FLG    =    SPACE
*            ヘッダ印字
*************PERFORM    HEAD-WT-SEC
*************MOVE  1    TO   PAGE-FLG
             MOVE  MAX-LINE TO  LINE-CNT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*倉庫ブレイク判定
     IF  DKC-F04   NOT =     WK-SOKCD
         MOVE      DKC-F04   TO   WK-SOKCD
         MOVE      DKC-F05   TO   WK-NOUHIN
         MOVE      1         TO   NOUHIN-FLG
         MOVE      DKC-F06   TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
         MOVE      1         TO   PAGE-FLG
     ELSE
         MOVE      ZERO      TO   DT1-03 DT1-04 DT1-05
         MOVE      SPACE     TO   DT1-HIFN1  DT1-HIFN2
     END-IF.
*納品日ブレイク判定
     IF  DKC-F05   NOT =     WK-NOUHIN
         MOVE      DKC-F05   TO   WK-NOUHIN
         MOVE      1         TO   NOUHIN-FLG
*
         MOVE      DKC-F06   TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
     ELSE
         MOVE      ZERO      TO   DT1-03 DT1-04 DT1-05
         MOVE      SPACE     TO   DT1-HIFN1  DT1-HIFN2
     END-IF.
*店舗ＣＤブレイク判定
     IF  DKC-F06   NOT =     WK-TENCD
         MOVE      DKC-F06   TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
     ELSE
         MOVE      SPACE     TO   DT1-01A
         MOVE      SPACE     TO   DT1-02
     END-IF
*
*
*明細出力
     PERFORM  BODY-WT-SEC.
*センター納品データ読込み
     PERFORM DSCENTF-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSCENTF HTOKMS TENMS1 PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7972L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  センター納品データ読込み                    *
*--------------------------------------------------------------*
 DSCENTF-READ-SEC            SECTION.
     READ   DSCENTF     NEXT
         AT END
            MOVE  "END"  TO  DKC-FLG
            MOVE  "END"  TO  END-FLG
            GO           TO  DSCENTF-READ-EXIT
         NOT   AT  END
            ADD    1     TO  DKC-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DKC-F01
     AND      PARA-JTIME  =  DKC-F02
     AND      PARA-TORICD =  DKC-F03
              CONTINUE
     ELSE
              MOVE     "END"     TO   END-FLG
              GO                 TO   DSCENTF-READ-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   DSCENTF-READ-EXIT
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =   DKC-F04
              GO                 TO   DSCENTF-READ-EXIT
     ELSE
              GO                 TO   DSCENTF-READ-SEC
     END-IF.
*
 DSCENTF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
*
***  ページ■
     ADD      1                   TO        PAGE-CNT.
     MOVE     PAGE-CNT            TO        HD1-04.
*    倉庫コード
     MOVE     DKC-F04             TO        HD2-01.
*    倉庫名取得
     MOVE     DKC-F04             TO        SOK-F01.
     PERFORM  900-SOK-READ.
     MOVE     SOK-F02             TO        HD2-TOKNM.
*    バッチ■編集
     MOVE     "<"                 TO        HD15-KU0.
     MOVE     PARA-JDATE(1:4)     TO        HD15-02YY.
     MOVE     "/"                 TO        HD15-KU1.
     MOVE     PARA-JDATE(5:2)     TO        HD15-02MM.
     MOVE     "/"                 TO        HD15-KU2.
     MOVE     PARA-JDATE(7:2)     TO        HD15-02DD.
     MOVE     PARA-JTIME(1:2)     TO        HD15-03HH.
     MOVE     ":"                 TO        HD15-KU3.
     MOVE     PARA-JTIME(3:2)     TO        HD15-03MM
     MOVE     PARA-TORICD         TO        HD15-04.
     MOVE    "-"                  TO        HD15-KU5  HD15-KU6.
     MOVE    ">"                  TO        HD15-KU4.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD15      AFTER  1.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     MOVE     8            TO     LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                明細印刷処理                                  *
*--------------------------------------------------------------*
 BODY-WT-SEC                SECTION.
*    MOVE    "BODY-WT-SEC"  TO   S-NAME.
*
*改ページ
*
*****IF  PAGE-CNT  >   1
         IF  LINE-CNT  >=  MAX-LINE
         OR  PAGE-FLG   =  1

             IF  PAGE-CNT   >  ZERO
                 MOVE   SPACE    TO     PRT-REC
                 WRITE  PRT-REC  AFTER  PAGE
             END-IF
*
             PERFORM  HEAD-WT-SEC
             MOVE     1         TO  PAGE-FLG
         END-IF.
*****END-IF.
*
     IF  NOUHIN-FLG   =  1
     AND  PAGE-FLG   =  0
*********WRITE     PRT-REC   FROM SEN  AFTER     1
         WRITE     PRT-REC   FROM SEN  AFTER     3
*********ADD       1         TO   LINE-CNT
         ADD       3         TO   LINE-CNT
     END-IF.
*
*帳票エリアクリア
*
*店舗の設定
     IF  PAGE-FLG   =  1
     OR  TENPO-FLG  =  1
          MOVE  DKC-F03        TO  TEN-F52
          MOVE  DKC-F06        TO  DT1-01   TEN-F011
          READ  TENMS1
              INVALID  KEY
                  MOVE  SPACE    TO  DT1-02
              NOT  INVALID  KEY
                  MOVE  TEN-F02  TO  DT1-02
          END-READ
     END-IF.
*
*納品日付取得
     IF  PAGE-FLG    =  1
     OR  NOUHIN-FLG  =  1
          MOVE    DKC-F05(1:4)        TO  DT1-03
          MOVE    "/"                 TO  DT1-HIFN1
          MOVE    DKC-F05(5:2)        TO  DT1-04
          MOVE    "/"                 TO  DT1-HIFN2
          MOVE    DKC-F05(7:2)        TO  DT1-05
     END-IF.
*
*部門
     MOVE     DKC-F07   TO   DT1-06.
**** EVALUATE     DKC-F07
******** WHEN    001
************* MOVE     NC"日曜大工"   TO   DT1-07
******** WHEN    002
************* MOVE     NC"建築建物"   TO   DT1-07
******** WHEN    003
************* MOVE     NC"木材・建材" TO   DT1-07
******** WHEN    004
************* MOVE     NC"インテリア" TO   DT1-07
******** WHEN    005
************* MOVE     NC"塗料接着剤" TO   DT1-07
******** WHEN    006
************* MOVE     NC"家庭用品"   TO   DT1-07
******** WHEN    007
************* MOVE     NC"住設配管"   TO   DT1-07
******** WHEN    008
************* MOVE     NC"園芸用品"   TO   DT1-07
******** WHEN    009
************* MOVE     NC"ペット用品" TO   DT1-07
******** WHEN    010
************* MOVE     NC"電気用品"   TO   DT1-07
******** WHEN    011
************* MOVE     NC"保安自転車" TO   DT1-07
******** WHEN    012
************* MOVE     NC"エクステリア"    TO    DT1-07
******** WHEN    013
************* MOVE     NC"カー用品"   TO   DT1-07
******** WHEN    014
************* MOVE     NC"文教用品"   TO   DT1-07
******** WHEN    015
************* MOVE     NC"花木"       TO   DT1-07
******** WHEN    016
************* MOVE     NC"日用雑貨"   TO   DT1-07
******** WHEN    018
************* MOVE     NC"収納用品"   TO   DT1-07
******** WHEN    019
************* MOVE     NC"ガーデン用品"    TO   DT1-07
******** WHEN    021
************* MOVE     NC"ライフ用品"  TO   DT1-07
******** WHEN    OTHER
************* MOVE     NC"＊＊＊＊＊＊"    TO   DT1-07
**** END-EVALUATE.
*
**08/02/20 STR
     EVALUATE     DKC-F07
         WHEN    001
              MOVE     NC"木材・建築資材"     TO   DT1-07
         WHEN    002
              MOVE     NC"園芸"               TO   DT1-07
         WHEN    003
              MOVE     NC"工具"               TO   DT1-07
         WHEN    004
              MOVE     NC"金物"               TO   DT1-07
         WHEN    005
              MOVE     NC"インテリア"         TO   DT1-07
         WHEN    006
              MOVE     NC"カー用品"           TO   DT1-07
         WHEN    007
              MOVE     NC"電材"               TO   DT1-07
         WHEN    008
              MOVE     NC"日用消耗"           TO   DT1-07
         WHEN    009
              MOVE     NC"スポーツ・玩具"     TO   DT1-07
         WHEN    010
              MOVE     NC"文具"               TO   DT1-07
         WHEN    011
              MOVE     NC"ガーデンＥＸ"       TO   DT1-07
         WHEN    012
              MOVE     NC"ペット"             TO   DT1-07
         WHEN    013
              MOVE     NC"植物"               TO   DT1-07
         WHEN    014
              MOVE     NC"家具・収納"         TO   DT1-07
         WHEN    015
              MOVE     NC"塗料・補修"         TO   DT1-07
         WHEN    016
              MOVE     NC"自転車・レジャー"   TO   DT1-07
         WHEN    017
              MOVE     NC"家電"               TO   DT1-07
         WHEN    018
              MOVE     NC"ダイニングキッチン" TO   DT1-07
         WHEN    019
              MOVE     NC"ソフトグッズ"       TO   DT1-07
         WHEN    020
              MOVE     NC"カウンター用品・サングラス・喫煙具"
                                              TO   DT1-07
         WHEN    022
              MOVE     NC"ＨＢＣ（ヘアケア・ボディ・車椅子）"
                                              TO   DT1-07
         WHEN    023
              MOVE     NC"作業用品"           TO   DT1-07
         WHEN    024
              MOVE     NC"リフォーム"         TO   DT1-07
         WHEN    025
              MOVE     NC"寝具"               TO   DT1-07
         WHEN    027
              MOVE     NC"照明"               TO   DT1-07
         WHEN    028
              MOVE     NC"バス・トイレタリー" TO   DT1-07
         WHEN    029
              MOVE     NC"ペットコンセ"       TO   DT1-07
         WHEN    030
              MOVE     NC"食品"               TO   DT1-07
         WHEN    031
              MOVE     NC"灯油"               TO   DT1-07
         WHEN    034
              MOVE     NC"催事"               TO   DT1-07
         WHEN    OTHER
              MOVE     NC"＊＊＊＊＊＊"       TO   DT1-07
     END-EVALUATE.
*08/02/20 END
*
     EVALUATE     DKC-F16
         WHEN    "1"
              MOVE     NC"【定番・送込み】"   TO   DT1-08
         WHEN    "2"
              MOVE     NC"【特売　　　　】"   TO   DT1-08
         WHEN    "3"
              MOVE     NC"【本部発注　　】"   TO   DT1-08
         WHEN    "4"
              MOVE     NC"【新店発注　　】"   TO   DT1-08
         WHEN    "5"
              MOVE     NC"【増床発注　　】"   TO   DT1-08
         WHEN    "6"
              MOVE     NC"【特注（客注）】"   TO   DT1-08
         WHEN    OTHER
              MOVE     NC"＊＊＊＊＊＊＊＊"   TO   DT1-08
     END-EVALUATE.
*
     MOVE     ZERO     TO  PAGE-FLG.
     MOVE     ZERO     TO  TENPO-FLG.
     MOVE     ZERO     TO  NOUHIN-FLG.
*
*****WRITE    PRT-REC   FROM      DT1  AFTER    1.
     WRITE    PRT-REC   FROM      DT1  AFTER    2.
     WRITE    PRT-REC   FROM     SEN1  AFTER    2.
*****ADD      1         TO   LINE-CNT.
     ADD      4         TO   LINE-CNT.
*
 BODY-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE     TO   TOK-F02
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-SOK-READ           SECTION.
     READ     ZSOKMS    INVALID
              MOVE      SPACE     TO   SOK-F02
     END-READ.
 900-SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　 READ                          *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     TENMS1    INVALID
              MOVE      SPACE     TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
