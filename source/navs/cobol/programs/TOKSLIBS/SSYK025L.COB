****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　出荷明細表                        *
*    モジュール名　　　　：　出荷明細表　出力（簡易）          *
*    作成日／更新日　　　：　2000/04/13                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷明細ワークを読み、出荷明細表  *
*                            を作表する。                      *
*                                                              *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSYK025L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       00.04.13.
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
*出荷明細ワーク
     SELECT   SYUMEIKF       ASSIGN        TO  01-S-SYUMEIKF
                             ORGANIZATION  IS  SEQUENTIAL
                             ACCESS MODE   IS  SEQUENTIAL
                             FILE STATUS   IS  SYU-STA.

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
 FD  SYUMEIKF        BLOCK CONTAINS   5  RECORDS.
     COPY     SYUMEIKF OF  XFDLIB
     JOINING  SYU      AS  PREFIX.
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
     03  SYU-STA             PIC  X(02).
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
     03  SYU-FLG             PIC  9(01)  VALUE  ZERO.
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
     03  WK-KBN1             PIC  X(05)  VALUE  SPACE.
     03  WK-KBN2             PIC  X(05)  VALUE  SPACE.
     03  WK-KBN3             PIC  X(10)  VALUE  SPACE.
     03  WK-TANA             PIC  X(06)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
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
         05  FILLER          PIC  X(12)  VALUE  "### SSYK025L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSYK025L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** SYUMEIKF ".
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
     03  FILLER     PIC  X(08)     VALUE  "SSYK025L".
     03  FILLER     PIC  X(11)     VALUE  SPACE.
     03  FILLER     PIC  N(11)     VALUE
         NC"※※　出荷明細表　※※" CHARACTER TYPE PIT-3.
     03  FILLER     PIC  X(12)     VALUE  SPACE.
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
 01  HEAD03.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"取引先："
                                   CHARACTER TYPE PIT-2.
     03  HD03-01    PIC  N(15)
                                   CHARACTER TYPE PIT-15.
 01  HEAD04.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
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
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"部　門："
                                   CHARACTER TYPE PIT-2.
     03  HD05-01    PIC  XXXX.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"店舗："
                                   CHARACTER TYPE PIT-2.
     03  HD05-02    PIC  9(05).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  HD05-03    PIC  N(15)     CHARACTER TYPE PIT-2.
 01  HEAD06                        CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"　棟　：".
     03  HD06-01    PIC  X.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"区分".
     03  FILLER     PIC  X(02)     VALUE  "1:".
     03  HD06-02    PIC  X(05).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"区分".
     03  FILLER     PIC  X(02)     VALUE  "2:".
     03  HD06-03    PIC  X(05).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"区分".
     03  FILLER     PIC  X(02)     VALUE  "3:".
     03  HD06-04    PIC  X(10).
*****03  FILLER     PIC  X(47)     VALUE  SPACE.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"出力順：".
     03  HD06-05    PIC  N(08).
 01  HEAD07.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(02)     VALUE  NC"_番"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(04)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"商品名"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(43)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"発注数量"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER     PIC  N(04)     VALUE  NC"訂正数量"
                                   CHARACTER TYPE PIT-2.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER     PIC  N(03)     VALUE  NC"売単価"
                                   CHARACTER TYPE PIT-2.
 01  MEISAI01.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI01-01   PIC  X(03).
     03  MEI01-SEN1 PIC  X(01).
     03  MEI01-02   PIC  X(02).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI01-03   PIC  N(01)     CHARACTER TYPE PIT-15.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI01-04   PIC  N(30)     CHARACTER TYPE PIT-15.
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI01-05   PIC  ZZZ,ZZ9.
     03  MEI01-SEN2 PIC  X(02).
     03  MEI01-06   PIC  ZZZ,ZZ9.
     03  MEI01-SEN3 PIC  X(03).
     03  MEI01-07   PIC  ZZZ,ZZ9.
 01  MEISAI02.
     03  FILLER     PIC  X(03)     VALUE  SPACE.
     03  MEI02-01   PIC  X(03).
     03  MEI02-SEN1 PIC  X(01).
     03  MEI02-02   PIC  X(02).
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI02-03   PIC  N(01)     CHARACTER TYPE PIT-15.
     03  FILLER     PIC  X(02)     VALUE  SPACE.
     03  MEI02-04   PIC  X(45).
     03  FILLER     PIC  X(01)     VALUE  SPACE.
     03  MEI02-05   PIC  ZZZ,ZZ9.
     03  MEI02-SEN2 PIC  X(02).
     03  MEI02-06   PIC  ZZZ,ZZZ.
     03  MEI02-SEN3 PIC  X(03).
     03  MEI02-07   PIC  ZZZ,ZZ9.
 01  SEN01.
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
     USE      AFTER  EXCEPTION   PROCEDURE       SYUMEIKF.
     MOVE    "SYUMEIKF"    TO    ERR-FL-ID.
     MOVE     SYU-STA      TO    ERR-STCD.
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
     DISPLAY  "**  SSYK025L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSYK025L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
     OPEN     INPUT       SYUMEIKF.
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
     IF  SYU-FLG   =  9
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
     IF  SYU-F15  NOT  =  WK-NOUHIN
         MOVE     SYU-F15     TO   WK-NOUHIN
         MOVE     1           TO   PAGE-FLG
     END-IF.
*倉庫ＣＤ ブレイク判定
     IF  SYU-F02  NOT  =  WK-SOKOCD
         MOVE     SYU-F02     TO   WK-SOKOCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*店舗区分ブレイク判定
*****IF  SYU-F13      NOT  =  WK-TENKB
*****    MOVE     SYU-F13     TO   WK-TENKB
*****    MOVE     1           TO   PAGE-FLG
*****END-IF.
*部門ブレイク判定
     IF  SYU-F04     NOT  =  WK-BUMON
         MOVE     SYU-F04     TO   WK-BUMON
         MOVE     1           TO   PAGE-FLG
     END-IF.
     IF  PARA-OUTNO     =     1
*        店舗ＣＤブレイク判定
         IF  SYU-F03      NOT  =  WK-TENCD
             MOVE     SYU-F03     TO   WK-TENCD
             MOVE     1           TO   PAGE-FLG
         END-IF
     END-IF.
*区分１ブレイク判定
     IF  SYU-F20         NOT   =   WK-KBN1
         MOVE     SYU-F20     TO   WK-KBN1
         MOVE     1           TO   PAGE-FLG
     END-IF.
*区分２ブレイク判定
     IF  SYU-F21         NOT   =   WK-KBN2
         MOVE     SYU-F21     TO   WK-KBN2
         MOVE     1           TO   PAGE-FLG
     END-IF.
*区分３ブレイク判定
     IF  SYU-F22         NOT   =   WK-KBN3
         MOVE     SYU-F22     TO   WK-KBN3
         MOVE     1           TO   PAGE-FLG
     END-IF.
*_番（上１桁）ブレイク判定
     IF  SYU-F05(1:1)    NOT   =   WK-TANA(1:1)
         MOVE     SYU-F05     TO   WK-TANA
         MOVE     1           TO   PAGE-FLG
     END-IF.
*明細出力
     PERFORM  BODY-WT-SEC.
*出荷明細ワークリード
     PERFORM  SYU-RD-SEC.
     IF  SYU-FLG   =  9
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
     MOVE    SYU-F011            TO  HD02-01.
     MOVE    SYU-F012            TO  HD02-02.
     MOVE    SYU-F013            TO  HD02-03.
*取引先名
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    SYU-F013      TO    TOK-F01
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
     MOVE  SYU-F04               TO  HD05-01.
     IF    PARA-OUTNO   =    1
*          店舗名の設定
           MOVE  SYU-F013        TO  TEN-F52
           MOVE  SYU-F03         TO  HD05-02  TEN-F011
           READ  TENMS1
              INVALID  KEY
                 MOVE  SPACE     TO  HD05-03
              NOT INVALID  KEY
                 MOVE  TEN-F02   TO  HD05-03
           END-READ
     END-IF.
*出力順の設定
     IF    PARA-OUTNO   =    1
           MOVE    NC"店舗＋部門＋_番"     TO   HD06-05
     ELSE
           IF      PARA-OUTNO    =     2
                   MOVE NC"部門＋_番毎集計"  TO HD06-05
           ELSE
                   MOVE SPACE               TO   HD06-05
           END-IF
     END-IF.
*区分１設定
     MOVE  SYU-F20              TO   HD06-02.
*区分２設定
     MOVE  SYU-F21              TO   HD06-03.
*区分３設定
     MOVE  SYU-F22              TO   HD06-04.
*_番（１：１）の設定
     MOVE  WK-TANA(1:1)         TO   HD06-01.
*印刷
     WRITE   PRT-REC   FROM   HEAD01   AFTER   2.
     WRITE   PRT-REC   FROM   HEAD02   AFTER   1
     WRITE   PRT-REC   FROM   HEAD03   AFTER   1
     WRITE   PRT-REC   FROM   HEAD04   AFTER   1
     WRITE   PRT-REC   FROM   HEAD05   AFTER   1
     WRITE   PRT-REC   FROM   HEAD06   AFTER   1
     WRITE   PRT-REC   FROM   SEN01    AFTER   1
     WRITE   PRT-REC   FROM   HEAD07   AFTER   1
*
     MOVE    10                  TO  LINE-CNT.
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
     MOVE  SPACE               TO  MEISAI01 MEISAI02.
*_番
     MOVE  SYU-F05(2:3)        TO  MEI01-01 MEI02-01.
     MOVE  SYU-F05(5:2)        TO  MEI01-02 MEI02-02.
     MOVE  "-"                 TO  MEI01-SEN1 MEI02-SEN1.
     MOVE  " ("                TO  MEI01-SEN2 MEI02-SEN2.
     MOVE  ")  "               TO  MEI01-SEN3 MEI02-SEN3.
*数量
     MOVE  SYU-F09             TO  MEI01-05 MEI02-05.
     IF    SYU-F09  NOT  =  SYU-F10
           MOVE  SYU-F10       TO  MEI01-06 MEI02-06
*****ELSE
*****      MOVE  ZERO          TO  MEI01-06 MEI02-06
     END-IF.
*売単価
     MOVE  SYU-F12             TO  MEI01-07 MEI02-07.
*
*PARA-OUTNO = 1 （店舗毎集計）時
     IF    PARA-OUTNO   =    1
***  引当済み印表示
           IF    SYU-F17     =    1
                 MOVE   SPACE    TO  MEI01-03 MEI02-03
           ELSE
                 MOVE   NC"★"   TO  MEI01-03 MEI02-03
           END-IF
     END-IF.
*商品名
     MOVE  SYU-F061            TO  SHO-F011.
     MOVE  SYU-F062            TO  SHO-F012.
     READ  MEIMS1
           INVALID
              MOVE  SYU-F081   TO  MEI02-04(1:15)
              MOVE  SYU-F082   TO  MEI02-04(16:15)
              MOVE  "INV"      TO  HMEIMS-INV-FLG
           NOT INVALID  KEY
              MOVE  SHO-F02    TO  MEI01-04
              MOVE  SPACE      TO  HMEIMS-INV-FLG
     END-READ.
*
     IF    HMEIMS-INV-FLG = "INV"
              WRITE PRT-REC FROM   MEISAI02 AFTER   2
     ELSE
              WRITE PRT-REC FROM   MEISAI01 AFTER   2
     END-IF.
*印刷
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                出荷明細ワークリード処理
*=============================================================
 SYU-RD-SEC      SECTION.
     MOVE    "SYU-RD-SEC"  TO  S-NAME.
*リード
     READ  SYUMEIKF      NEXT
         AT   END
           MOVE   9        TO  SYU-FLG
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
     CLOSE      SYUMEIKF.
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
