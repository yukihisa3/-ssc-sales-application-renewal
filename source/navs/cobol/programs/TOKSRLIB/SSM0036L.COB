****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　出荷業務　出荷明細書              *
*    モジュール名　　　　：　出荷明細表（ケース数算出）        *
*    作成日／更新日　　　：　2018/08/27                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷明細ワークを読み、出荷明細表  *
*                            を作表する。                      *
*    作成日／更新日　　　：　2019/04/23                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ファイルキー変更　　　　　　　　　*
*                            　　　　　　                      *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSM0036L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       14.09.17.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     YA           IS     PIT-2
     YB           IS     PIT-15
     YB-21        IS     PIT-3
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*出荷明細ワーク（ケース数算出）
     SELECT   SYKMEIL2 ASSIGN     TO       DA-01-VI-SYKMEIL2
                        ORGANIZATION       IS  INDEXED
                        ACCESS    MODE     IS  SEQUENTIAL
                        RECORD    KEY      IS  SYK-F01   SYK-F02
                                               SYK-F04   SYK-F03
                                               SYK-F15
                                               SYK-F19   SYK-F23
                                               SYK-F24   SYK-F25
                                               SYK-F05   SYK-F07
                        FILE      STATUS   IS  SYK-STA.
*店舗Ｍ
     SELECT   TENMS1         ASSIGN        TO  01-VI-TENMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TEN-F52  TEN-F011
                             FILE STATUS   IS  TEN-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
     SELECT   MEIMS1         ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHO-F011
                                               SHO-F012
                             FILE STATUS   IS  TEN-STA.

*取引先Ｍ
     SELECT   TOKMS2         ASSIGN        TO  01-VI-TOKMS2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TOK-F01
                             FILE STATUS   IS  TOK-STA.

*プリンタＦ
     SELECT   PRTF           ASSIGN        TO  LP-04
                             FILE STATUS   IS  PRT-STA.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*出荷明細ワーク
 FD  SYKMEIL2.
     COPY     SYKMEIL2 OF  XFDLIB
     JOINING  SYK      AS  PREFIX.
*店舗Ｍ
 FD  TENMS1.
     COPY     HTENMS   OF  XFDLIB
     JOINING  TEN      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS1.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  SHO      AS  PREFIX.
*取引先Ｍ
 FD  TOKMS2.
     COPY     HTOKMS   OF  XFDLIB
     JOINING  TOK      AS  PREFIX.
*プリンタＦ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  SYK-STA             PIC  X(02).
     03  TEN-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  SHO-STA             PIC  X(02).
     03  TOK-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  WK-SYSDTW           PIC  9(08)  VALUE  ZERO.
     03  WK-SYSDTW-R         REDEFINES   WK-SYSDTW.
       05  WK-SYSDT-YY       PIC  9(04).
       05  WK-SYSDT-MM       PIC  9(02).
       05  WK-SYSDT-DD       PIC  9(02).
     03  FLG-TOK             PIC  9(01)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  SYK-FLG             PIC  9(01)  VALUE  ZERO.
     03  PAGE-FLG            PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
     03  WK-NOUHIN-R         REDEFINES   WK-NOUHIN.
       05  WK-NOUHIN-YY      PIC  9(04).
       05  WK-NOUHIN-MM      PIC  9(02).
       05  WK-NOUHIN-DD      PIC  9(02).
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
     03  WK-TENKB            PIC  X(01)  VALUE  SPACE.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-BUMON            PIC  X(04)  VALUE  SPACE.
     03  WK-TANA             PIC  X(06)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  WK-SYUBETU1         PIC  X(02)  VALUE  SPACE.
     03  WK-SYUBETU2         PIC  X(01)  VALUE  SPACE.
     03  WK-SYUBETU3         PIC  X(01)  VALUE  SPACE.
     03  WK-BUTURYU          PIC  X(05)  VALUE  SPACE.
*ﾊﾞｯﾁNO.
 01  WK-BATCH.
     03  WK-JDATE            PIC  9(08)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-JTIME            PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-TORI             PIC  9(08)  VALUE  ZERO.
*メッセージ情報
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE  " *** ".
         05  FILLER          PIC  X(07)  VALUE  " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SSM0036L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSM0036L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** SYKMEIL2 ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-PRINT.
         05  FILLER          PIC  X(13)  VALUE  "*** PRTF     ".
         05  FILLER          PIC  X(10)  VALUE  "  PAGE  = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
*ﾁｮｳﾋｮｳﾃｲｷﾞｴﾘｱ
 01  HEAD01.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  X(08)     VALUE  "SSM0036L".
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(16)     VALUE
         NC"●●出荷明細表（ケース単位）●●"
         CHARACTER TYPE PIT-3.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  HD01-01    PIC  9999.
     03  FILLER     PIC  N(01)     VALUE  NC"年"
                                   CHARACTER TYPE PIT-2.
     03  HD01-02    PIC  Z9.
     03  FILLER     PIC  N(01)     VALUE  NC"月"
                                   CHARACTER TYPE PIT-2.
     03  HD01-03    PIC  Z9.
     03  FILLER     PIC  N(01)     VALUE  NC"日"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  HD01-04    PIC  ZZ9.
     03  FILLER     PIC  N(01)     VALUE  NC"頁"
                                   CHARACTER TYPE PIT-2.
 01  HEAD02.
     03  FILLER     PIC  X(22)     VALUE  SPACE.
     03  FILLER     PIC  X(08)     VALUE  "(ﾊﾞｯﾁNO ".
     03  HD02-01    PIC  9(08).
     03  FILLER     PIC  X(01)     VALUE  "-".
     03  HD02-02    PIC  9(04).
     03  FILLER     PIC  X(01)     VALUE  "-".
     03  HD02-03    PIC  9(08).
     03  FILLER     PIC  X(01)     VALUE  ")".
     03  FILLER     PIC  X(16)     VALUE  SPACE.
     03  HD02-04    PIC  X(08).
 01  HEAD03.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"取引先："
                                   CHARACTER TYPE PIT-2.
     03  HD03-01    PIC  N(15)
                                   CHARACTER TYPE PIT-15.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(05)     VALUE  NC"物流区分："
                                   CHARACTER TYPE PIT-2.
     03  HD03-02    PIC  X(03).
 01  HEAD04.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"倉　庫："
                                   CHARACTER TYPE PIT-2.
     03  HD04-01    PIC  N(18)     CHARACTER TYPE PIT-15.
     03  FILLER     PIC  X(21)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"納品日："
                                   CHARACTER TYPE PIT-2.
     03  HD04-02    PIC  9999.
     03  FILLER     PIC  N(01)     VALUE  NC"年"
                                   CHARACTER TYPE PIT-2.
     03  HD04-03    PIC  Z9.
     03  FILLER     PIC  N(01)     VALUE  NC"月"
                                   CHARACTER TYPE PIT-2.
     03  HD04-04    PIC  Z9.
     03  FILLER     PIC  N(01)     VALUE  NC"日"
                                   CHARACTER TYPE PIT-2.
 01  HEAD05.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"部　門："
                                   CHARACTER TYPE PIT-2.
     03  HD05-01    PIC  XXXX.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"店舗："
                                   CHARACTER TYPE PIT-2.
     03  HD05-02    PIC  9(05).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  HD05-03    PIC  N(15)     CHARACTER TYPE PIT-2.
 01  HEAD06.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"　棟　："
                                   CHARACTER TYPE PIT-2.
     03  HD06-01    PIC  X.
     03  FILLER     PIC  X(47)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"出力順："
                                   CHARACTER TYPE PIT-2.
     03  HD06-02    PIC  N(08)     CHARACTER TYPE PIT-2.
 01  HEAD065                       CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(05)     VALUE  NC"発注種別：".
     03  HD065-01   PIC  XX.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  HD065-02   PIC  N(08).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"形状：".
     03  HD065-03   PIC  X.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  HD065-04   PIC  N(03).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(05)     VALUE  NC"発注単位：".
     03  HD065-05   PIC  X.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  HD065-06   PIC  N(03).
 01  HEAD07.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"_番"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(01)     VALUE  NC"欠"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"商品名"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(30)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"入数"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"発注数"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"訂正数"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"ケース数"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"バラ数"
                                   CHARACTER TYPE PIT-2.
 01  MEISAI01.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI01-01   PIC  X(03).
     03  MEI01-SEN1 PIC  X(01).
     03  MEI01-02   PIC  X(02).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI01-03   PIC  N(01)     CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI01-04   PIC  N(15)     CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI01-05S  PIC  X(01).
     03  MEI01-05   PIC  ZZ,ZZ9.
     03  MEI01-05E  PIC  X(01).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI01-06   PIC  ZZ,ZZ9.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI01-07   PIC  ZZ,ZZ9.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI01-08S  PIC  N(01)     CHARACTER TYPE PIT-2.
     03  MEI01-08   PIC  ZZ,ZZ9.
     03  MEI01-08E  PIC  N(01)     CHARACTER TYPE PIT-2.
     03  MEI01-09S  PIC  X(01)     VALUE  SPACE.
     03  MEI01-09   PIC  ZZ,ZZ9.
     03  MEI01-09E  PIC  X(01)     VALUE  SPACE.
 01  MEISAI02.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI02-01   PIC  X(03).
     03  MEI02-SEN1 PIC  X(01).
     03  MEI02-02   PIC  X(02).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI02-03   PIC  N(01)     CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI02-04   PIC  X(30).
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI02-05S  PIC  X(01).
     03  MEI02-05   PIC  ZZ,ZZ9.
     03  MEI02-05E  PIC  X(01).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI02-06   PIC  ZZ,ZZ9.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI02-07   PIC  ZZ,ZZ9.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI02-08S  PIC  N(01)     CHARACTER TYPE PIT-2.
     03  MEI02-08   PIC  ZZ,ZZ9.
     03  MEI02-08E  PIC  N(01)     CHARACTER TYPE PIT-2.
     03  MEI02-09S  PIC  X(01)     VALUE  SPACE.
     03  MEI02-09   PIC  ZZ,ZZ9.
     03  MEI02-09E  PIC  X(01)     VALUE  SPACE.
 01  MEISAI03.
     03  FILLER     PIC  X(13)     VALUE  SPACE.
     03  MEI03-01   PIC  N(15)     CHARACTER TYPE PIT-2.
     03  MEI03-02S  PIC  X(01)     VALUE  SPACE.
     03  MEI03-02   PIC  X(13)     VALUE  SPACE.
     03  MEI03-02E  PIC  X(01)     VALUE  SPACE.
 01  SEN01.
     03  FILLER         OCCURS    90.
         05  FILLER     PIC  X(01)     VALUE  "=".
 01  SEN02.
     03  FILLER         OCCURS    90.
         05  FILLER     PIC  X(01)     VALUE  "-".
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*=============================================================
 LINKAGE             SECTION.
*=============================================================
 01  PARA-OUTNO              PIC  9(01).
******************************************************************
 PROCEDURE               DIVISION      USING    PARA-OUTNO.
******************************************************************
 DECLARATIVES.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*出荷明細ワーク
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       SYKMEIL2.
     MOVE    "SYKMEIL2"    TO    ERR-FL-ID.
     MOVE     SYK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*店舗Ｍ
 TEN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TENMS1.
     MOVE    "TENMS1"      TO    ERR-FL-ID.
     MOVE     TEN-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*商品名称Ｍ
 SYU-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     SHO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*取引先Ｍ
 TOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TOKMS2.
     MOVE    "TOKMS2"      TO    ERR-FL-ID.
     MOVE     TOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SSM0036L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSM0036L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
     OPEN     INPUT       SYKMEIL2.
     OPEN     INPUT       TENMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     INPUT       TOKMS2.
     OPEN     OUTPUT      PRTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-SYSDT    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-SYSDTW
     ELSE
         MOVE    ZERO           TO   WK-SYSDTW
     END-IF.
*ＭＡＸ行設定
     MOVE     63          TO    MAX-LINE.
*出荷明細ワークリード
     PERFORM  SYU-RD-SEC.
     IF  SYK-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     MOVE    "MAIN-SEC"       TO   S-NAME.
*納品日ブレイク判定
     IF  SYK-F15  NOT  =  WK-NOUHIN
         MOVE     SYK-F15     TO   WK-NOUHIN
         MOVE     1           TO   PAGE-FLG
     END-IF.
*物流区分ブレイク判定
     IF  SYK-F19  NOT  =  WK-BUTURYU
         MOVE     SYK-F19     TO   WK-BUTURYU
         MOVE     1           TO   PAGE-FLG
     END-IF.
*倉庫ＣＤ ブレイク判定
     IF  SYK-F02  NOT  =  WK-SOKOCD
         MOVE     SYK-F02     TO   WK-SOKOCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*部門ブレイク判定
     IF  SYK-F04     NOT  =  WK-BUMON
         MOVE     SYK-F04     TO   WK-BUMON
         MOVE     1           TO   PAGE-FLG
     END-IF.
*発注種別区分／形状区分／発注単位区分
     IF  SYK-F23     NOT  =  WK-SYUBETU1
         MOVE     SYK-F23      TO  WK-SYUBETU1
         MOVE     1            TO  PAGE-FLG
     END-IF.
     IF  SYK-F24     NOT  =  WK-SYUBETU2
         MOVE     SYK-F24      TO  WK-SYUBETU2
         MOVE     1            TO  PAGE-FLG
     END-IF.
     IF  SYK-F25     NOT  =  WK-SYUBETU3
         MOVE     SYK-F25      TO  WK-SYUBETU3
         MOVE     1            TO  PAGE-FLG
     END-IF.
*_番（上１桁）ブレイク判定
     IF  SYK-F05(1:1)    NOT   =   WK-TANA(1:1)
         MOVE     SYK-F05     TO   WK-TANA
         MOVE     1           TO   PAGE-FLG
     END-IF.
*店舗ＣＤブレイク判定
     IF  SYK-F03         NOT   =   WK-TENCD
         MOVE     SYK-F03     TO   WK-TENCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*出荷明細ワークリード
     PERFORM  SYU-RD-SEC.
     IF  SYK-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     MOVE    "HEAD-WT-SEC"       TO  S-NAME.
*日付
     MOVE    WK-SYSDT-YY         TO  HD01-01.
     MOVE    WK-SYSDT-MM         TO  HD01-02.
     MOVE    WK-SYSDT-DD         TO  HD01-03.
*頁
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  HD01-04.
*ﾊﾞｯﾁNO.
     MOVE    SYK-F011            TO  HD02-01.
     MOVE    SYK-F012            TO  HD02-02.
     MOVE    SYK-F013            TO  HD02-03.
*取引先名
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    SYK-F013      TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F02   TO        HD03-01
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F02   TO        HD03-01
     END-IF.
*物流区分
     MOVE    SYK-F19           TO    HD03-02.
*納品日
     MOVE    WK-NOUHIN-YY        TO  HD04-02.
     MOVE    WK-NOUHIN-MM        TO  HD04-03.
     MOVE    WK-NOUHIN-DD        TO  HD04-04.
*倉庫名の設定
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE        TO  HD04-01
           NOT INVALID  KEY
              MOVE  SOK-F02      TO  HD04-01
     END-READ.
*部門の設定
     MOVE  SYK-F04               TO  HD05-01.
     IF    PARA-OUTNO   =    1
*          店舗名の設定
           MOVE  SYK-F013        TO  TEN-F52
           MOVE  SYK-F03         TO  HD05-02  TEN-F011
           MOVE  WK-TENCD        TO  HD05-02  TEN-F011
           READ  TENMS1
              INVALID  KEY
                 MOVE  SPACE     TO  HD05-03
              NOT INVALID  KEY
                 MOVE  TEN-F02   TO  HD05-03
           END-READ
     END-IF.
*出力順の設定
     MOVE  NC"店舗＋部門＋_番"  TO  HD06-02.
*発注種別区分
     MOVE  SYK-F23              TO   HD065-01.
     EVALUATE  SYK-F23
         WHEN  "00"  MOVE NC"定期　　　　　" TO HD065-02
         WHEN  "01"  MOVE NC"特売　　　　　" TO HD065-02
         WHEN  "02"  MOVE NC"新店改装　　　" TO HD065-02
         WHEN  "03"  MOVE NC"投入　　　　　" TO HD065-02
         WHEN  "04"  MOVE NC"緊急ダイレクト" TO HD065-02
         WHEN  "05"  MOVE NC"客注ダイレクト" TO HD065-02
         WHEN  "06"  MOVE NC"新規初回　　　" TO HD065-02
         WHEN  "51"  MOVE NC"本部在庫補充　" TO HD065-02
         WHEN  "52"  MOVE NC"専用_在庫補充" TO HD065-02
         WHEN  "71"  MOVE NC"用度品　　　　" TO HD065-02
         WHEN  "81"  MOVE NC"プロモーション" TO HD065-02
         WHEN  "82"  MOVE NC"改廃　　　　　" TO HD065-02
         WHEN  "83"  MOVE NC"ＢＹ改廃　　　" TO HD065-02
         WHEN  OTHER MOVE NC"＊＊＊＊＊＊＊" TO HD065-02
     END-EVALUATE.
*形状区分
     MOVE  SYK-F24              TO   HD065-03.
     EVALUATE  SYK-F24
         WHEN  "1"   MOVE NC"小物"           TO HD065-04
         WHEN  "2"   MOVE NC"異形"           TO HD065-04
         WHEN  "3"   MOVE NC"ケース"         TO HD065-04
*#2018/02/19 NAV ST
         WHEN  "4"   MOVE NC"大カゴ"         TO HD065-04
*#2018/02/19 NAV ED
         WHEN  OTHER MOVE NC"他　　"         TO HD065-04
     END-EVALUATE.
*発注単位区分
     MOVE  SYK-F25              TO   HD065-05.
     EVALUATE  SYK-F25
         WHEN  "P"   MOVE NC"ピース"         TO HD065-06
         WHEN  "C"   MOVE NC"ケース"         TO HD065-06
         WHEN  OTHER MOVE NC"？？？"         TO HD065-06
     END-EVALUATE.
*_番（１：１）の設定
     MOVE  WK-TANA(1:1)         TO   HD06-01.
*印刷
     WRITE   PRT-REC   FROM   HEAD01   AFTER   2.
     WRITE   PRT-REC   FROM   HEAD02   AFTER   1.
     WRITE   PRT-REC   FROM   HEAD03   AFTER   1.
     WRITE   PRT-REC   FROM   HEAD04   AFTER   1.
     WRITE   PRT-REC   FROM   HEAD05   AFTER   1.
     IF  SYK-F013 = 880 OR 881 OR 882 OR 1427 OR 14272 OR 14273
     OR  13938 OR 139381 OR 17137 OR 171371 OR 1731 OR 1732
     OR  7601 OR 7602
          WRITE   PRT-REC   FROM   HEAD065  AFTER   1
          MOVE    11               TO  LINE-CNT
     ELSE
          MOVE    10               TO  LINE-CNT
     END-IF.
     WRITE   PRT-REC   FROM   HEAD06   AFTER   1.
     WRITE   PRT-REC   FROM   SEN01    AFTER   1.
     WRITE   PRT-REC   FROM   HEAD07   AFTER   1.
*
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE    "BODY-WT-SEC"     TO  S-NAME.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-FLG   =  1
         IF  PAGE-CNT   >  ZERO
             MOVE   SPACE      TO  PRT-REC
             WRITE  PRT-REC  AFTER  PAGE
         END-IF
         PERFORM  HEAD-WT-SEC
         MOVE     ZERO         TO  PAGE-FLG
     END-IF.
*帳票エリアクリア
     MOVE  SPACE               TO  MEISAI01 MEISAI02 MEISAI03.
*_番
     MOVE  SYK-F05(2:3)        TO  MEI01-01 MEI02-01.
     MOVE  SYK-F05(5:2)        TO  MEI01-02 MEI02-02.
     MOVE  "-"                 TO  MEI01-SEN1 MEI02-SEN1.
     MOVE  "("                 TO  MEI01-05S  MEI02-05S.
     MOVE  ")"                 TO  MEI01-05E  MEI02-05E.
     MOVE  NC"【"              TO  MEI01-08S  MEI02-08S.
     MOVE  NC"】"              TO  MEI01-08E  MEI02-08E.
     MOVE  "("                 TO  MEI01-09S  MEI02-09S.
     MOVE  ")"                 TO  MEI01-09E  MEI02-09E.
     MOVE  "("                 TO  MEI03-02S.
     MOVE  ")"                 TO  MEI03-02E.
*入数
     MOVE  SYK-F20             TO  MEI01-05 MEI02-05.
*発注数
     MOVE  SYK-F09             TO  MEI01-06 MEI02-06.
*訂正数（発注数と異なる場合のみに訂正数は出力する。）
     IF  SYK-F09  NOT =  SYK-F10
         MOVE  SYK-F10         TO  MEI01-07 MEI02-07
     END-IF.
*ケース数
     MOVE  SYK-F21             TO  MEI01-08 MEI02-08.
*バラ数
     MOVE  SYK-F22             TO  MEI01-09 MEI02-09.
*
***  引当済み印表示
     IF    SYK-F17     =    1
           MOVE   SPACE        TO  MEI01-03 MEI02-03
     ELSE
           MOVE   NC"★"       TO  MEI01-03 MEI02-03
     END-IF.
*商品名
     IF  SYK-F083  =  SPACE
     AND SYK-F084  =  SPACE
         MOVE  SYK-F081        TO  MEI02-04(1:15)
         MOVE  SYK-F082        TO  MEI02-04(16:15)
         MOVE  SPACE           TO  MEI03-01
     ELSE
         MOVE  SYK-F083        TO  MEI01-04
         MOVE  SYK-F084        TO  MEI03-01
     END-IF.
*ＪＡＮＣＤセット
     MOVE  SYK-F07             TO  MEI03-02.
*
     IF  SYK-F083  =  SPACE
     AND SYK-F084  =  SPACE
              WRITE PRT-REC FROM   MEISAI02 AFTER   1
              WRITE PRT-REC FROM   MEISAI03 AFTER   1
     ELSE
              WRITE PRT-REC FROM   MEISAI01 AFTER   1
              WRITE PRT-REC FROM   MEISAI03 AFTER   1
     END-IF.
*印刷
     IF       PARA-OUTNO  =  "1" OR "2"
              WRITE PRT-REC FROM   SEN02    AFTER   1
              ADD   3                     TO  LINE-CNT
     ELSE
              ADD   2                     TO  LINE-CNT
     END-IF.
*
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                出荷明細ワークリード処理
*=============================================================
 SYU-RD-SEC      SECTION.
     MOVE    "SYU-RD-SEC"  TO  S-NAME.
*リード
     READ  SYKMEIL2      NEXT
         AT   END
           MOVE   9        TO  SYK-FLG
           GO              TO  SYU-RD-EXIT
     END-READ.
     ADD   1               TO         CNT-READ.
 SYU-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
     MOVE      "END-SEC"     TO     S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE      PRTF.
     CLOSE      SYKMEIL2.
     CLOSE      TENMS1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      TOKMS2.
*ＭＳＧ出力
     DISPLAY    MSG-INPUT    UPON   CONS.
     MOVE       PAGE-CNT     TO     CNT-WRITE.
     DISPLAY    MSG-PRINT    UPON   CONS.
 END-EXIT.
     EXIT.
