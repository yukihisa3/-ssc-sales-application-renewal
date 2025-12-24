****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　コーナン　　　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　箱数ファイル作成リスト発行　　　　*
*    作成日／作成者　　　：　2021/12/16 INOUE                  *
*    処理概要　　　　　　：　箱数ファイルデータを読み、　　　　*
*                        　　箱数ファイル作成リストを発行。　　*
*                        　　ピッキング・梱包数記入用。　　　　*
*                        　　（カトーレック）　　　　　　　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                        　　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY1370L.
*                  流用:SSY9103L.TOKSLIBS
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/12/16.
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
*----<<箱数ファイル　>>----*
     SELECT   KNHAKOF   ASSIGN    TO        DA-01-VI-KNHAKOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAK-F001  HAK-F002
                                            HAK-F003  HAK-F004
                                            HAK-FA05  HAK-FA01
                                            HAK-FA02  HAK-FA03
                                            HAK-FA06  HAK-FA04
                        FILE      STATUS    HAK-ST.
*----<< 取引先マスタ >>--*
*    SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       TOK-F01
*                       FILE      STATUS    TOK-ST.
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
*#2019/03/18 NAV ST
*発注種別変換マスタ
*    SELECT   DCMHSBL2  ASSIGN    TO        DA-01-VI-DCMHSBL2
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       HSB-F01  HSB-F03
*                       FILE  STATUS   IS   HSB-ST.
*#2019/03/18 NAV ED
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<箱数ファイル　　　　　　>>----*
 FD  KNHAKOF            LABEL RECORD   IS   STANDARD.
     COPY     KNHAKOF   OF        XFDLIB
              JOINING   HAK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 取引先マスタ >>--*
*FD  HTOKMS             LABEL RECORD   IS   STANDARD.
*    COPY     HTOKMS    OF        XFDLIB
*             JOINING   TOK       PREFIX.
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
*#2019/03/18 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
*FD  DCMHSBL2           LABEL RECORD   IS   STANDARD.
*    COPY     DCMHSBF   OF        XFDLIB
*             JOINING   HSB       PREFIX.
*
*#2019/03/18 NAV ED
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  HAK-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
     03  PAGE-FLG       PIC  9(01)   VALUE ZERO.
     03  TENPO-FLG      PIC  9(01)   VALUE ZERO.
     03  NOUHIN-FLG     PIC  9(01)   VALUE ZERO.
*#2019/03/11 NAV ST
*01  DCMHSBL2-INV-FLG   PIC  X(03)   VALUE  SPACE.
*#2019/03/11 NAV ED
 01  WK-CNT.
     03  HAK-CNT        PIC  9(07).
     03  TOK-CNT        PIC  9(07).
     03  TEN-CNT        PIC  9(07).
     03  PAGE-CNT       PIC  9(03).
     03  LINE-CNT       PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HAK-ST         PIC  X(02).
     03  TOK-ST         PIC  X(02).
     03  TEN-ST         PIC  X(02).
     03  SOK-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*#2019/03/18 NAV ST
*    03  HSB-ST         PIC  X(02).
*#2019/03/18 NAV ED
*
 01  MAX-LINE          PIC  9(02)      VALUE  52.
 01  PG-ID             PIC  X(08)      VALUE  "SSY1370L".
 01  WK-MSG1           PIC  N(14)
                       VALUE NC"指定のデータはありません。".
*----<< ﾜｰｸ >>--*
 01  WK-AREA.
   03  WK-TENCD        PIC  9(05).
   03  WK-NOUHIN       PIC  9(06).
   03  WK-SOKCD        PIC  X(02).
 01  CK-NDATE.
     03  CK-NDATE1         PIC 9(02).
     03  CK-NDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
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
     03  FILLER                  PIC  X(15)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"　※※　"
                                 CHARACTER  TYPE  IS  CHR-21.
*    03  HD1-TOKNM               PIC  N(03)
*                                CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  N(16)  VALUE
                            NC"　箱数ファイル作成リスト　※※　"
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
     03  FILLER                  PIC  X(45)  VALUE  SPACE.
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
     03  HD2-SOKNM               PIC  N(10)
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  HD3.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
*
     03  FILLER                  PIC  N(03)  VALUE  NC"納品日"
                                 CHARACTER   TYPE  IS  CHR-2.
*
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
*
*    03  FILLER                  PIC  N(01)  VALUE
*                                NC"店"
*                                CHARACTER   TYPE  IS  CHR-2.
*
*    03  FILLER                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
*
     03  FILLER                  PIC  N(03)  VALUE  NC"納品先"
                                 CHARACTER   TYPE  IS  CHR-2.
*
     03  FILLER                  PIC  X(31)  VALUE  SPACE.
*
     03  FILLER                  PIC  N(03)  VALUE  NC"ルート"
                                 CHARACTER   TYPE  IS  CHR-2.
*
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
     03  FILLER                  PIC  N(02)  VALUE  NC"部門"
                                 CHARACTER   TYPE  IS  CHR-2.
*
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
*
     03  FILLER                  PIC  N(03)  VALUE  NC"発注日"
                                 CHARACTER   TYPE  IS  CHR-2.
*
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
*
     03  FILLER                  PIC  N(02)  VALUE  NC"箱数"
                                 CHARACTER   TYPE  IS  CHR-2.
*
*    03  FILLER                  PIC  X(07)  VALUE  SPACE.
*
*    03  FILLER                  PIC  N(10)  VALUE
*                                NC"　ケース　　　　　他"
*                                CHARACTER   TYPE  IS  CHR-2.
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
         "                                                  ".
     03  FILLER                  PIC  X(50)  VALUE
         " -------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  NDATE-Y                 PIC  ZZZZ.
     03  DT1-HIFN1               PIC  X.
     03  NDATE-M                 PIC  ZZ.
     03  DT1-HIFN2               PIC  X.
     03  NDATE-D                 PIC  ZZ.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  NCODE-A.
       05  NCODE                 PIC  9999.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  NNAME                   PIC  N(15).
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  ROUTE                   PIC  X(02).
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  BUMON                   PIC  X(02).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HDATE-Y                 PIC  ZZZZ.
     03  DT1-HIFN3               PIC  X.
     03  HDATE-M                 PIC  ZZ.
     03  DT1-HIFN4               PIC  X.
     03  HDATE-D                 PIC  ZZ.
*    03  DT1-07                  PIC  N(12).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  X(12)  VALUE
     "(          )".
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-08                  PIC  N(08).
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
 01  PARA-NDATE             PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NDATE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HAKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KNHAKOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1370L HAK ERROR " HAK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*得意先マスタ
*TOKERR                SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
*    ACCEPT   SYS-DATE       FROM DATE.
*    ACCEPT   SYS-TIME       FROM TIME.
*    DISPLAY  "### SSY1370L TOK ERROR " TOK-ST " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
*             SYS-HH ":" SYS-MN ":" SYS-SS " ###"
*                                      UPON CONS.
*    STOP     RUN.
*店舗マスタ
 TENERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1370L TEN ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*倉庫マスタ
 SOKERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1370L SOK ERROR " SOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1370L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*HSBERR                 SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      DCMHSBL2.
*    ACCEPT   SYS-DATE       FROM DATE.
*    ACCEPT   SYS-TIME       FROM TIME.
*    MOVE     4000           TO   PROGRAM-STATUS.
*    DISPLAY  "### SSY8852L DCMHSBL2 ERROR " HSB-ST " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
*             SYS-HH ":" SYS-MN ":" SYS-SS " ###"
*                                      UPON CONS.
*    STOP     RUN.
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
     DISPLAY  "*** SSY1370L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     KNHAKOF   TENMS1   ZSOKMS.
*#2019/03/18 NAV ST
*    OPEN     INPUT     DCMHSBL2.
*#2019/03/18 NAV ED
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
     MOVE     PARA-NDATE     TO   CK-NDATE.
*箱数ファイルスタート
     MOVE     SPACE          TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE     PARA-JDATE     TO   HAK-F001.
     MOVE     PARA-JTIME     TO   HAK-F002.
     MOVE     PARA-TORICD    TO   HAK-F003.
     MOVE     PARA-SOKO      TO   HAK-F004.
     MOVE     PARA-NDATE     TO   WK-NDATE.
     MOVE     WK-NDATE2      TO   HAK-FA05.
     MOVE     ZERO           TO   HAK-FA01.
     MOVE     ZERO           TO   HAK-FA02.
     MOVE     ZERO           TO   HAK-FA03.
     MOVE     ZERO           TO   HAK-FA06.
     MOVE     ZERO           TO   HAK-FA04.
     START    KNHAKOF   KEY  >=   HAK-F001   HAK-F002
                                  HAK-F003   HAK-F004
                                  HAK-FA05   HAK-FA01
                                  HAK-FA02   HAK-FA03
                                  HAK-FA06   HAK-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              DISPLAY NC"＃＃出力対象無し！！＃＃" UPON CONS
              GO   TO   100-INIT-RTN-EXIT
     END-START.
*    箱数ファイル読込み
     PERFORM KNHAKOF-READ-SEC.
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
     IF  HAK-F004  NOT =     WK-SOKCD
         MOVE      HAK-F004  TO   WK-SOKCD
         MOVE      HAK-FA05  TO   WK-NOUHIN
         MOVE      1         TO   NOUHIN-FLG
         MOVE      HAK-FA01  TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
         MOVE      1         TO   PAGE-FLG
     ELSE
         MOVE      ZERO      TO   NDATE-Y NDATE-M NDATE-D
         MOVE      SPACE     TO   DT1-HIFN1  DT1-HIFN2
*                                 DT1-HIFN3  DT1-HIFN4
     END-IF.
*納品日ブレイク判定
     IF  HAK-FA05  NOT =     WK-NOUHIN
         MOVE      HAK-FA05  TO   WK-NOUHIN
         MOVE      1         TO   NOUHIN-FLG
*
         MOVE      HAK-FA01  TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
     ELSE
         MOVE      ZERO      TO   NDATE-Y NDATE-M NDATE-D
         MOVE      SPACE     TO   DT1-HIFN1  DT1-HIFN2
*                                 DT1-HIFN3  DT1-HIFN4
     END-IF.
*納品先ブレイク判定
     IF  HAK-FA01  NOT =     WK-TENCD
         MOVE      HAK-FA01  TO   WK-TENCD
         MOVE      1         TO   TENPO-FLG
     ELSE
         MOVE      SPACE     TO   NCODE-A
         MOVE      SPACE     TO   NNAME
     END-IF
*
*
*明細出力
     PERFORM BODY-WT-SEC.
*箱数ファイル読込み
     PERFORM KNHAKOF-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    KNHAKOF TENMS1 PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "    PAGE = "  PAGE-CNT "    " UPON CONS.
     DISPLAY  "*** SSY1370L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  箱数ファイル読込み                    *
*--------------------------------------------------------------*
 KNHAKOF-READ-SEC            SECTION.
     READ   KNHAKOF     NEXT
         AT END
            MOVE  "END"  TO  HAK-FLG
            MOVE  "END"  TO  END-FLG
            GO           TO  KNHAKOF-READ-EXIT
         NOT   AT  END
            ADD    1     TO  HAK-CNT
     END-READ.
 KNHAKOF-READ-01.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  HAK-F001
     AND      PARA-JTIME  =  HAK-F002
     AND      PARA-TORICD =  HAK-F003
              CONTINUE
     ELSE
              MOVE     "END"     TO   END-FLG
              GO                 TO   KNHAKOF-READ-EXIT
     END-IF.
 KNHAKOF-READ-02.
*    倉庫チェック
     IF       PARA-SOKO   =  SPACE
              GO                 TO   KNHAKOF-READ-03
     END-IF.
     IF       PARA-SOKO      =   HAK-F004
              GO                 TO   KNHAKOF-READ-03
     ELSE
              GO                 TO   KNHAKOF-READ-SEC
     END-IF.
 KNHAKOF-READ-03.
*    納品日チェック
     IF       PARA-NDATE  =  ZERO
              GO                 TO   KNHAKOF-READ-EXIT
     END-IF.
*T↓
*    DISPLAY "CK-NDATE2=" CK-NDATE UPON CONS.
*    DISPLAY "HAK-FA05 =" HAK-FA05 UPON CONS.
*T↑
     IF       CK-NDATE2   =  HAK-FA05
*T↓
*             DISPLAY "=" UPON CONS
*T↑
              GO                 TO   KNHAKOF-READ-EXIT
     ELSE
*T↓
*             DISPLAY "NOT=" UPON CONS
*T↑
              GO                 TO   KNHAKOF-READ-SEC
     END-IF.
*
 KNHAKOF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  取引先名
*    IF  HAK-F003  =  13938  OR  17137  OR  139381  OR  171371
*             MOVE NC"カーマ"     TO        HD1-TOKNM
*    END-IF.
*    IF  HAK-F003  =  100403 OR  100427  OR  100441
*                 OR 100404 OR  100428  OR  100442
*             MOVE NC"ダイキ"     TO        HD1-TOKNM
*    END-IF.
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
     MOVE     HAK-F004            TO        HD2-01.
*    倉庫名取得
     MOVE     HAK-F004            TO        SOK-F01.
     PERFORM  900-SOK-READ.
     MOVE     SOK-F02             TO        HD2-SOKNM.
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
*帳票編集
*
*納品日
     IF  PAGE-FLG    =  1
     OR  NOUHIN-FLG  =  1
*         MOVE    HAK-FA05(1:4)       TO  NDATE-Y
*         MOVE    "/"                 TO  DT1-HIFN1
*         MOVE    HAK-FA05(5:2)       TO  NDATE-M
*         MOVE    "/"                 TO  DT1-HIFN2
*         MOVE    HAK-FA05(7:2)       TO  NDATE-D
          MOVE     "3"                TO  LINK-IN-KBN
          MOVE     HAK-FA05           TO  LINK-IN-YMD6
          MOVE     ZERO               TO  LINK-IN-YMD8
          MOVE     ZERO               TO  LINK-OUT-RET
          MOVE     ZERO               TO  LINK-OUT-YMD
          CALL     "SKYDTCKB"      USING  LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD
          MOVE     LINK-OUT-YMD(1:4)  TO  NDATE-Y
          MOVE     "/"                TO  DT1-HIFN1
          MOVE     LINK-OUT-YMD(5:2)  TO  NDATE-M
          MOVE     "/"                TO  DT1-HIFN2
          MOVE     LINK-OUT-YMD(7:2)  TO  NDATE-D
     END-IF.
*
*納品先の設定
     IF  PAGE-FLG   =  1
     OR  TENPO-FLG  =  1
          MOVE  HAK-F003        TO  TEN-F52
          MOVE  HAK-FA01        TO  NCODE   TEN-F011
          READ  TENMS1
              INVALID  KEY
                  MOVE  SPACE    TO  NNAME
              NOT  INVALID  KEY
                  MOVE  TEN-F02  TO  NNAME
          END-READ
     END-IF.
*
*ルート
     MOVE     HAK-FA03  TO   ROUTE.
*部門
     MOVE     HAK-FA06  TO   BUMON.
*
*発注日
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     HAK-FA04            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HDATE-Y.
     MOVE     "/"                 TO        DT1-HIFN3.
     MOVE     LINK-OUT-YMD(5:2)   TO        HDATE-M.
     MOVE     "/"                 TO        DT1-HIFN4.
     MOVE     LINK-OUT-YMD(7:2)   TO        HDATE-D.
*
     MOVE     ZERO     TO  PAGE-FLG.
     MOVE     ZERO     TO  TENPO-FLG.
     MOVE     ZERO     TO  NOUHIN-FLG.
*
     WRITE    PRT-REC   FROM      DT1  AFTER    2.
     WRITE    PRT-REC   FROM     SEN1  AFTER    2.
     ADD      4         TO   LINE-CNT.
*
 BODY-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
*900-TOK-READ           SECTION.
*    READ     HTOKMS    INVALID
*             MOVE      SPACE     TO   TOK-F02
*    END-READ.
*900-TOK-READ-EXIT.
*    EXIT.
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
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
*DCMHSBL2-READ-SEC         SECTION.
*
*    READ     DCMHSBL2
*        INVALID
*          MOVE  "INV"     TO        DCMHSBL2-INV-FLG
*        NOT INVALID
*          MOVE  SPACE     TO        DCMHSBL2-INV-FLG
*    END-READ.
*
*DCMHSBL2-READ-EXIT.
*    EXIT.
