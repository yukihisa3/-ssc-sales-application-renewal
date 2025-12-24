# SSY7520V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7520V.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細データＣＳＶ出力　　　　　*
*    作成日／更新日　　　：　2015/12/11                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭサンワ支払明細データよりＤ  *
*                        ：　ＣＭサンワの支払明細データＣＳＶ  *
*                        ：　を出力する。　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SSY7520V.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/12/03.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
*    STATION     IS        STA
     CONSOLE     IS        CONS.
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< ＤＣＭサンワ支払明細Ｄ >>--*
     SELECT   SWSIHAF   ASSIGN         DA-01-VI-SWSIHAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F01
                                       SIH-F02
                                       SIH-F03
                                       SIH-F06
                                       SIH-F04
                        STATUS         SIH-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TEN-ST.
*----<< ＥＤＩＣ名称マスタ >>--*
     SELECT   EDMEISF   ASSIGN         DA-01-VI-EDMEISL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MES-F01
                                       MES-F02
                                       MES-F04
                        STATUS         MES-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        STATUS         JYO-ST.
*----<< 支払明細データＣＳＶ >>--*
     SELECT   SWSIHAWK     ASSIGN    TO  DA-01-S-SWSIHAWK
                           FILE      STATUS    SWK-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< ＤＣＭサンワ支払明細 >>--*
 FD  SWSIHAF     LABEL       RECORD    IS        STANDARD.
     COPY        SWSIHAF   OF        XFDLIB
     JOINING     SIH       AS        PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS      LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS      LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*----<< ＥＤＩＣ名称マスタ >>--*
 FD  EDMEISF     LABEL       RECORD    IS        STANDARD.
     COPY        EDMEISF   OF        XFDLIB
     JOINING     MES       AS        PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*----<< 支払明細データＣＳＶ >>--*
 FD  SWSIHAWK           BLOCK        CONTAINS  1    RECORDS.
*
 01 CSV-REC.
    03  FILLER                    PIC  X(1000).
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
*条件ファイルレコード退避用
     COPY   HJYOKEN  OF XFDLIB  JOINING   JWW  AS   PREFIX.
*----<< ファイルステータス >>--*
 01  FILE-STATUS.
     03  SIH-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  MES-ST              PIC  X(02).
     03  JYO-ST              PIC  X(02).
     03  SWK-ST              PIC  X(02).
*
*----<< 変数 >>--*
 01  WK-AREA.
     03  END-SW              PIC  9(01)  VALUE   ZERO.
     03  ERR-SW              PIC  9(01)  VALUE   ZERO.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
     03  WK-ZEINUKI          PIC S9(10)  VALUE   ZERO.
     03  WK-GENKAK           PIC S9(10)  VALUE   ZERO.
     03  WK-GENKAN           PIC S9(10)  VALUE   ZERO.
     03  WK-ZEI              PIC  9(03)  VALUE   ZERO.
*
*----<< 集計変数 >>--*
*
 01  TKEI-AREA.
     03  GENKEIK             PIC S9(10)  VALUE   ZERO.
     03  GENKEIN             PIC S9(10)  VALUE   ZERO.
*
 01  SKEI-AREA.
     03  SOGKEIK             PIC S9(10)  VALUE   ZERO.
     03  SOGKEIN             PIC S9(10)  VALUE   ZERO.
*
*----<< ブレイク項目 >>--*
 01  NEW-KEY.
     02  NEW-TENCD           PIC  9(05).
*
 01  OLD-KEY.
     02  OLD-TENCD           PIC  9(05).
*
*----<< 検収日変換 >>--*
 01  KENSYUBI.
     03  KENSYUBI-NEN        PIC  9(04).
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  KENSYUBI-TSUKI      PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  KENSYUBI-HI         PIC  9(02).
*----<< 支払締年月変換 >>--*
 01  SIMEYMD.
     03  SIME-YYYY           PIC  9(04).
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  SIME-MM             PIC  9(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
*
 01  IN-DATA                 PIC  X(01).
 01  FILE-ERR.
     03  SIH-ERR             PIC  N(10) VALUE
         NC"支払明細データエラー".
     03  TOK-ERR             PIC  N(10) VALUE
         NC"取引先マスタ　エラー".
     03  TEN-ERR             PIC  N(10) VALUE
         NC"店舗マスタ　　エラー".
     03  MES-ERR             PIC  N(10) VALUE
         NC"ＥＤＩＣ名称Ｍエラー".
     03  JYO-ERR             PIC  N(10) VALUE
         NC"条件ファイル　エラー".
     03  SWK-ERR             PIC  N(10) VALUE
         NC"支払明細ＣＳＶエラー".
*
 01  READ-CNT                PIC  9(07) VALUE  0.
 01  IN-CNT                  PIC  9(07) VALUE  0.
 01  SET-FLG                 PIC  X(03) VALUE  SPACE.
 01  HENKAN-FLG              PIC  X(03) VALUE  SPACE.
 01  DEN-FLG                 PIC  9(01) VALUE  0.
 01  HIT-FLG                 PIC  9(01) VALUE  0.
 01  HTOKMS-INV-FLG          PIC  X(03) VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03) VALUE  SPACE.
 01  EDMEISF-INV-FLG         PIC  X(03) VALUE  SPACE.
*ＣＳＶ出力定義************************************************
 01  WK-CSV-001.
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(13)
                       VALUE NC"＜ＤＣＭサンワ支払明細書＞".
     03  FILLER              PIC  X(01) VALUE  X"29".
*
 01  WK-CSV-002.
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"支払締日".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  HD-SIMEBI1          PIC  X(04).
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(01) VALUE  NC"年".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  HD-SIMEBI2          PIC  X(02).
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(02) VALUE  NC"月度".
     03  FILLER              PIC  X(01) VALUE  X"29".
*
 01  WK-CSV-003.
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(03) VALUE NC"取引先".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  HD-TOKCD            PIC  9(08).
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  HD-TOKNM            PIC  N(15).
     03  FILLER              PIC  X(01) VALUE  X"29".
*
 01  WK-CSV-004.
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"店舗ＣＤ".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(03) VALUE NC"店舗名".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(03) VALUE NC"検収日".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"伝票番号".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(02) VALUE NC"部門".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"伝票区分".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"伝票種類".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"１行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"１行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"２行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"２行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"３行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"３行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"４行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"４行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"５行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"５行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"６行目数量".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(05) VALUE NC"６行目原価".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER PIC  N(10) VALUE NC"原価金額合計（税込）".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER PIC  N(10) VALUE NC"原価金額合計（税抜）".
     03  FILLER              PIC  X(01) VALUE  X"29".
     03  FILLER              PIC  X(01) VALUE  ",".
     03  FILLER              PIC  X(01) VALUE  X"28".
     03  FILLER              PIC  N(04) VALUE NC"消費税率".
     03  FILLER              PIC  X(01) VALUE  X"29".
*
 01  WK-CSV-005.
     03  MS-TENCD            PIC  9(05).
     03  MS-KU01             PIC  X(01).
     03  MS-TENMEI-1         PIC  X(01).
     03  MS-TENMEI           PIC  N(15).
     03  MS-TENMEI-2         PIC  X(01).
     03  MS-KU02             PIC  X(01).
     03  MS-KENSYUBI         PIC  X(10).
     03  MS-KU03             PIC  X(01).
     03  MS-DENNO            PIC  9(09).
     03  MS-KU04             PIC  X(01).
     03  MS-BUMON            PIC  X(02).
     03  MS-KU05             PIC  X(01).
     03  MS-DENKU            PIC  X(02).
     03  MS-DENKU-1          PIC  X(01).
     03  MS-DENKU-2          PIC  X(01).
     03  MS-DENKUNM          PIC  N(05).
     03  MS-DENKU-3          PIC  X(01).
     03  MS-KU06             PIC  X(01).
     03  MS-DENSYU           PIC  X(01).
     03  MS-DENSYU-1         PIC  X(01).
     03  MS-DENSYU-2         PIC  X(01).
     03  MS-DENSYUNM         PIC  N(04).
     03  MS-DENSYU-3         PIC  X(01).
     03  MS-KU07             PIC  X(01).
     03  MS-MEISAI           OCCURS  6.
         05  MS-SURYO        PIC  -----9.9.
         05  MS-KU08         PIC  X(01).
         05  MS-GENKA        PIC  --------9.9.
         05  MS-KU09         PIC  X(01).
     03  MS-GENKEIK          PIC  ---------9.
     03  MS-KU10             PIC  X(01).
     03  MS-GENKEIN          PIC  ---------9.
     03  MS-KU11             PIC  X(01).
*****03  MS-SYOHIZEI-0       PIC  X(01).
*****03  MS-SYOHIZEI-1       PIC  X(01).
     03  MS-SYOHIZEI         PIC  ZZ9.
*****03  MS-SYOHIZEI-2       PIC  X(01).
     03  MS-SYOHIZEIN        PIC  X(01).
*****03  MS-SYOHIZEI-3       PIC  X(01).
*****03  MS-SYOHIZEI-4       PIC  X(01).
*
****************************************************************
 LINKAGE                   SECTION.
 01  PARA-TOKCD              PIC  9(08).
 01  PARA-NENGETU            PIC  9(06).
****************************************************************
 PROCEDURE                 DIVISION  USING
                                     PARA-TOKCD  PARA-NENGETU.
****************************************************************
 DECLARATIVES.
 SWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SWSIHAWK.
     DISPLAY     SWK-ERR   UPON      CONS.
     DISPLAY     SWK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     STOP        RUN.
 SIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SWSIHAF.
     DISPLAY     SIH-ERR   UPON      CONS.
     DISPLAY     SIH-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MES-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EDMEISF.
     DISPLAY     MES-ERR   UPON      CONS.
     DISPLAY     MES-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     MAIN-SEC
                 UNTIL END-SW = 1.
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ
**********************************************************
 INIT-SEC                  SECTION.
*
*----<< システム日付取得 >>--*
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE      20                 TO   WK-YS.
     INITIALIZE                        TKEI-AREA
                                       SKEI-AREA.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*
*----<< システム時刻取得 >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN      INPUT   SWSIHAF  HTOKMS  HTENMS  EDMEISF  HJYOKEN.
     OPEN      OUTPUT  SWSIHAWK.
*条件ファイル索引
     MOVE      SPACE              TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE      99                 TO   JYO-F01.
     MOVE     "ZEI"               TO   JYO-F02.
     READ  HJYOKEN
           INVALID
           MOVE   4000            TO   PROGRAM-STATUS
           DISPLAY NC"＃＃条件Ｆ（消費税率取得）エラー＃＃"
           UPON CONS
           STOP  RUN
           NOT  INVALID
           MOVE   JYO-REC         TO   JWW-REC
     END-READ.
*
     MOVE     SPACE               TO   SIH-REC.
     INITIALIZE                        SIH-REC.
     MOVE     PARA-NENGETU        TO   SIH-F01.
     MOVE     PARA-TOKCD          TO   SIH-F02.
     START  SWSIHAF  KEY  IS  >=  SIH-F01  SIH-F02  SIH-F03
                                  SIH-F06  SIH-F04
            INVALID
            MOVE     1            TO   END-SW
            DISPLAY NC"＃＃出力対象が存在しません！ＳＴ＃＃"
                                  UPON CONS
            GO                    TO   INIT-EXIT
     END-START.
*
*----<< １レコード目読込 >>--*
     PERFORM     SWSIHAF-READ-SEC.
*
     IF  END-SW  =  1
         DISPLAY NC"＃＃出力対象が存在しません！ＲＤ＃＃"
                                  UPON CONS
         GO                       TO   INIT-EXIT
     ELSE
*********タイトル行
         MOVE    SPACE            TO   CSV-REC
         MOVE    WK-CSV-001       TO   CSV-REC
         WRITE   CSV-REC
         MOVE    1                TO   LINE-CNT
*********支払締日行
         MOVE    SPACE            TO   CSV-REC
         MOVE    PARA-NENGETU(1:4) TO  HD-SIMEBI1
         MOVE    PARA-NENGETU(5:2) TO  HD-SIMEBI2
         MOVE    WK-CSV-002       TO   CSV-REC
         WRITE   CSV-REC
         ADD     1                TO   LINE-CNT
*********取引先行
         MOVE    SPACE            TO   CSV-REC
         MOVE    PARA-TOKCD       TO   HD-TOKCD
         MOVE    PARA-TOKCD       TO   TOK-F01
         PERFORM  HTOKMS-READ-SEC
         IF  HTOKMS-INV-FLG = SPACE
             MOVE  TOK-F03        TO   HD-TOKNM
         ELSE
             MOVE  ALL NC"＊"     TO   HD-TOKNM
         END-IF
         MOVE    WK-CSV-003       TO   CSV-REC
         WRITE   CSV-REC
         ADD     1                TO   LINE-CNT
*********ヘッダタイトル行
         MOVE    SPACE            TO   CSV-REC
         MOVE    WK-CSV-004       TO   CSV-REC
         WRITE   CSV-REC
         ADD     1                TO   LINE-CNT
     END-IF.
*
*----<< ブレイクしないように制御 >>--*
     MOVE     SIH-F03            TO   OLD-TENCD.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 MAIN-SEC                  SECTION.
*
*----<< ブレイク判定 >>--*
     IF    NEW-KEY    NOT =  OLD-KEY
           PERFORM     TENWRT-SEC
           MOVE   NEW-KEY    TO    OLD-KEY
     END-IF.
*
*----<< 集計処理 >>--*
     PERFORM     SYUKEI-SEC.
*
*----<< 明細出力 >>--*
     PERFORM     MEIWRT-SEC.
*
*----<< 次レコード読込 >>--*
     PERFORM     SWSIHAF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ
**********************************************************
 END-SEC                   SECTION.
*
*----<< 合計欄出力 >>--*
     IF    IN-CNT      >   ZERO
           PERFORM     TENWRT-SEC
           PERFORM     SOUWRT-SEC
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE       SWSIHAF  HTOKMS  HTENMS  EDMEISF  HJYOKEN.
     CLOSE       SWSIHAWK.
*
 END-EXIT.
     EXIT.
**********************************************************
*                    集計処理
**********************************************************
 SYUKEI-SEC                   SECTION.
*----<< 店舗計 >>--*
*    原価金額算出
     COMPUTE      WK-GENKAK  =  SIH-F113   /  10.
*
     IF  SIH-F112  =  "-"
         COMPUTE  WK-GENKAK  =  WK-GENKAK  *  -1
     END-IF.
*    消費税抜き算出
     IF  JWW-F06 <= SIH-F06
         COMPUTE WK-GENKAN ROUNDED = WK-GENKAK / JWW-F07
         COMPUTE WK-ZEI = JWW-F08 * 100
     ELSE
         COMPUTE WK-GENKAN ROUNDED = WK-GENKAK / JWW-F04
         COMPUTE WK-ZEI = JWW-F05 * 100
     END-IF.
*店舗計／総合計に加算
     ADD       WK-GENKAK          TO        GENKEIK.
     ADD       WK-GENKAK          TO        SOGKEIK.
     ADD       WK-GENKAN          TO        GENKEIN.
     ADD       WK-GENKAN          TO        SOGKEIN.
*
 SYUKEI-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*
*----<< クリア >>--*
     MOVE      SPACE              TO        WK-CSV-005.
*
     MOVE      ","            TO        MS-KU01  MS-KU02  MS-KU03
                                        MS-KU04  MS-KU05  MS-KU06
                                        MS-KU07  MS-KU10  MS-KU11.
*----<< 店舗ＣＤ／店舗名 >>--*
     MOVE  SIH-F02            TO        TEN-F52.
     MOVE  SIH-F03            TO        MS-TENCD   TEN-F011.
     MOVE  X"28"              TO        MS-TENMEI-1.
     MOVE  X"29"              TO        MS-TENMEI-2.
     PERFORM  HTENMS-READ-SEC
     IF  HTENMS-INV-FLG = SPACE
         MOVE  TEN-F03        TO        MS-TENMEI
     ELSE
         MOVE  ALL NC"＊"     TO        MS-TENMEI
     END-IF.
*検収日
     MOVE  SIH-F06(1:4)       TO        MS-KENSYUBI(1:4).
     MOVE  SIH-F06(5:2)       TO        MS-KENSYUBI(6:2).
     MOVE  SIH-F06(7:2)       TO        MS-KENSYUBI(9:2).
     MOVE  "/"                TO        MS-KENSYUBI(5:1).
     MOVE  "/"                TO        MS-KENSYUBI(8:1).
*伝票番号
     MOVE  SIH-F04            TO        MS-DENNO.
*部門
     MOVE  SIH-F109           TO        MS-BUMON.
*伝票区分
     MOVE  SIH-F110           TO        MS-DENKU.
*伝票区分名
     MOVE  ":"                TO        MS-DENKU-1.
     MOVE  ZERO               TO        MES-F01.
     MOVE  "DCM002"           TO        MES-F02.
     MOVE  SIH-F110           TO        MES-F04.
     MOVE  X"28"              TO        MS-DENKU-2.
     MOVE  X"29"              TO        MS-DENKU-3.
     PERFORM EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
           MOVE  MES-F05      TO        MS-DENKUNM
     ELSE
           MOVE  ALL NC"＊"   TO        MS-DENKUNM
     END-IF.
*伝票種類
     MOVE  SIH-F111           TO        MS-DENSYU.
*伝票種類名
     MOVE  ":"                TO        MS-DENSYU-1.
     MOVE  ZERO               TO        MES-F01.
     MOVE  "DCM003"           TO        MES-F02.
     MOVE  SIH-F111           TO        MES-F04.
     MOVE  X"28"              TO        MS-DENSYU-2.
     MOVE  X"29"              TO        MS-DENSYU-3.
     PERFORM EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
           MOVE  MES-F05      TO        MS-DENSYUNM
     ELSE
           MOVE  ALL NC"＊"   TO        MS-DENSYUNM
     END-IF.
     MOVE  ","                TO        MS-KU08(1).
     MOVE  ","                TO        MS-KU08(2).
     MOVE  ","                TO        MS-KU08(3).
     MOVE  ","                TO        MS-KU08(4).
     MOVE  ","                TO        MS-KU08(5).
     MOVE  ","                TO        MS-KU08(6).
     MOVE  ","                TO        MS-KU09(1).
     MOVE  ","                TO        MS-KU09(2).
     MOVE  ","                TO        MS-KU09(3).
     MOVE  ","                TO        MS-KU09(4).
     MOVE  ","                TO        MS-KU09(5).
     MOVE  ","                TO        MS-KU09(6).
*１行目数量／原価
     IF  SIH-F203 = "-"
         COMPUTE MS-SURYO(1)   =  (SIH-F205 / 10) * -1
         COMPUTE MS-GENKA(1)   =  (SIH-F204 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(1)   =  (SIH-F205 / 10)
         COMPUTE MS-GENKA(1)   =  (SIH-F204 / 10)
     END-IF.
     IF  SIH-F206 = "-"
         COMPUTE MS-SURYO(2)   =  (SIH-F208 / 10) * -1
         COMPUTE MS-GENKA(2)   =  (SIH-F207 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(2)   =  (SIH-F208 / 10)
         COMPUTE MS-GENKA(2)   =  (SIH-F207 / 10)
     END-IF.
     IF  SIH-F209 = "-"
         COMPUTE MS-SURYO(3)   =  (SIH-F211 / 10) * -1
         COMPUTE MS-GENKA(3)   =  (SIH-F210 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(3)   =  (SIH-F211 / 10)
         COMPUTE MS-GENKA(3)   =  (SIH-F210 / 10)
     END-IF.
     IF  SIH-F212 = "-"
         COMPUTE MS-SURYO(4)   =  (SIH-F214 / 10) * -1
         COMPUTE MS-GENKA(4)   =  (SIH-F213 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(4)   =  (SIH-F214 / 10)
         COMPUTE MS-GENKA(4)   =  (SIH-F213 / 10)
     END-IF.
     IF  SIH-F215 = "-"
         COMPUTE MS-SURYO(5)   =  (SIH-F217 / 10) * -1
         COMPUTE MS-GENKA(5)   =  (SIH-F216 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(5)   =  (SIH-F217 / 10)
         COMPUTE MS-GENKA(5)   =  (SIH-F216 / 10)
     END-IF.
     IF  SIH-F218 = "-"
         COMPUTE MS-SURYO(6)   =  (SIH-F220 / 10) * -1
         COMPUTE MS-GENKA(6)   =  (SIH-F219 / 10) * -1
     ELSE
         COMPUTE MS-SURYO(6)   =  (SIH-F220 / 10)
         COMPUTE MS-GENKA(6)   =  (SIH-F219 / 10)
     END-IF.
*原価金額合計（税込）
     MOVE  WK-GENKAK          TO        MS-GENKEIK.
     MOVE  WK-GENKAN          TO        MS-GENKEIN.
*税率
*****MOVE  "'"                TO        MS-SYOHIZEI-0.
*****MOVE  "("                TO        MS-SYOHIZEI-1.
     MOVE  WK-ZEI             TO        MS-SYOHIZEI.
*****MOVE  X"28"              TO        MS-SYOHIZEI-2.
     MOVE  "%"                TO        MS-SYOHIZEIN.
*****MOVE  X"29"              TO        MS-SYOHIZEI-3.
*****MOVE  ")"                TO        MS-SYOHIZEI-4.
*
*----<< 明細出力 >>--*
     MOVE    SPACE            TO        CSV-REC.
     MOVE    WK-CSV-005       TO        CSV-REC.
     WRITE   CSV-REC
*
     ADD   1                  TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 TENWRT-SEC                   SECTION.
*
*----<< クリア >>--*
     MOVE      SPACE          TO        WK-CSV-005.
*
     MOVE      ","            TO        MS-KU01  MS-KU02  MS-KU03
                                        MS-KU04  MS-KU05  MS-KU06
                                        MS-KU07  MS-KU10  MS-KU11.
     MOVE  ","                TO        MS-KU08(1).
     MOVE  ","                TO        MS-KU08(2).
     MOVE  ","                TO        MS-KU08(3).
     MOVE  ","                TO        MS-KU08(4).
     MOVE  ","                TO        MS-KU08(5).
     MOVE  ","                TO        MS-KU08(6).
     MOVE  ","                TO        MS-KU09(1).
     MOVE  ","                TO        MS-KU09(2).
     MOVE  ","                TO        MS-KU09(3).
     MOVE  ","                TO        MS-KU09(4).
     MOVE  ","                TO        MS-KU09(5).
     MOVE  ","                TO        MS-KU09(6).
*----<<店舗合計セット>>--*
     MOVE      X"28"          TO        MS-TENMEI-1.
     MOVE      NC"＜店舗計＞" TO        MS-TENMEI.
     MOVE      X"29"          TO        MS-TENMEI-2.
*
*----<< 項目セット >>--*
     MOVE      GENKEIK        TO        MS-GENKEIK.
     MOVE      GENKEIN        TO        MS-GENKEIN.
*
*----<< 出力 >>--*
*
     MOVE    SPACE            TO        CSV-REC.
     MOVE    WK-CSV-005       TO        CSV-REC.
*****WRITE   CSV-REC
*
     ADD   1                  TO        LINE-CNT.
*----<< 集計変数クリア >>--*
     INITIALIZE                             TKEI-AREA.
*
 TENWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 SOUWRT-SEC                   SECTION.
*
*----<< クリア >>--*
     MOVE      SPACE          TO        WK-CSV-005.
*
     MOVE      ","            TO        MS-KU01  MS-KU02  MS-KU03
                                        MS-KU04  MS-KU05  MS-KU06
                                        MS-KU07  MS-KU10  MS-KU11.
     MOVE  ","                TO        MS-KU08(1).
     MOVE  ","                TO        MS-KU08(2).
     MOVE  ","                TO        MS-KU08(3).
     MOVE  ","                TO        MS-KU08(4).
     MOVE  ","                TO        MS-KU08(5).
     MOVE  ","                TO        MS-KU08(6).
     MOVE  ","                TO        MS-KU09(1).
     MOVE  ","                TO        MS-KU09(2).
     MOVE  ","                TO        MS-KU09(3).
     MOVE  ","                TO        MS-KU09(4).
     MOVE  ","                TO        MS-KU09(5).
     MOVE  ","                TO        MS-KU09(6).
*
*----<<店舗合計セット>>--*
     MOVE      X"28"          TO        MS-TENMEI-1.
     MOVE      NC"＜支払総合計＞" TO        MS-TENMEI.
     MOVE      X"29"          TO        MS-TENMEI-2.
*
*----<< 項目セット >>--*
     MOVE      SOGKEIK        TO        MS-GENKEIK.
     MOVE      SOGKEIN        TO        MS-GENKEIN.
*
*----<< 出力 >>--*
*
     MOVE    SPACE            TO        CSV-REC.
     MOVE    WK-CSV-005       TO        CSV-REC.
*****WRITE   CSV-REC.
*
 SOUWRT-EXIT.
     EXIT.
****************************************************************
*             ワーク読込み
****************************************************************
 SWSIHAF-READ-SEC             SECTION.
*
*----<< ワーク読込 >>--*
     READ     SWSIHAF  NEXT  AT  END
              MOVE     1         TO   END-SW
              GO                 TO   SWSIHAF-READ-EXIT
     END-READ.
*
*----<< 仕入締年月チェック >>--*
     IF  PARA-NENGETU  NOT =  SIH-F01
              MOVE     1         TO   END-SW
              GO                 TO   SWSIHAF-READ-EXIT
     END-IF.
*
*----<< 取引先ＣＤチェック >>--*
     IF  PARA-TOKCD    NOT =  SIH-F02
              MOVE     1         TO   END-SW
              GO                 TO   SWSIHAF-READ-EXIT
     END-IF.
*
*----<< ブレイク項目セット >>--*
     MOVE     SIH-F03            TO   NEW-TENCD.
*
*----<< 読込件数カウントアップ >>--*
     ADD      1                  TO   IN-CNT.
*
 SWSIHAF-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC              SECTION.
*
     READ  HTOKMS
           INVALID     MOVE "INV"     TO   HTOKMS-INV-FLG
           NOT INVALID MOVE SPACE     TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             店舗マスタ読込
****************************************************************
 HTENMS-READ-SEC              SECTION.
*
     READ  HTENMS
           INVALID     MOVE "INV"     TO   HTENMS-INV-FLG
           NOT INVALID MOVE SPACE     TO   HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＤＩＣ名称マスタ読込
****************************************************************
 EDMEISF-READ-SEC             SECTION.
*
     READ  EDMEISF
           INVALID     MOVE "INV"     TO   EDMEISF-INV-FLG
           NOT INVALID MOVE SPACE     TO   EDMEISF-INV-FLG
     END-READ.
*
 EDMEISF-READ-EXIT.
     EXIT.

```
