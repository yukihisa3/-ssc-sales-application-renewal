# SSY7521L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7521L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細書発行　　　　　　　　　　*
*    作成日／更新日　　　：　2015/12/03                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭサンワ支払明細データよりＤ  *
*                        ：　ＣＭサンワの支払明細書を発行する  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SSY7521L.
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
*----<< 帳票ファイル >>--*
     SELECT   PRTFILE   ASSIGN         01-GS-PRTF
                        DESTINATION    "PRT"
                        FORMAT         PRT-FMT
                        GROUP          PRT-GRP
                        PROCESSING     PRT-PRO
                        CONTROL        PRT-CTL
                        STATUS         PRT-ST
                                       PRT-ST2.
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
*----<< 帳票ファイル >>--*
 FD  PRTFILE.
     COPY        FSY75202  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
*条件ファイルレコード退避用
     COPY   HJYOKEN  OF XFDLIB  JOINING   JWW  AS   PREFIX.
*----<< 帳票パラメータ >>--*
 01  PRT-AREA.
     03  PRT-FMT            PIC  X(08).
     03  PRT-PRO            PIC  X(02).
     03  PRT-GRP            PIC  X(08).
     03  PRT-CTL.
         05  PRT-CNTRL      PIC  X(04).
         05  PRT-STR-PG     PIC  X(02).
     03  PRT-ST             PIC  X(02).
     03  PRT-ST2            PIC  X(04).
*----<< 固定項目 >>--*
 01  MAX-LINE                PIC  9(02)  VALUE   50.
*
*----<< ファイルステータス >>--*
 01  FILE-STATUS.
     03  SIH-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  MES-ST              PIC  X(02).
     03  JYO-ST              PIC  X(02).
*
*----<< 変数 >>--*
 01  WK-AREA.
     03  END-SW              PIC  9(01)  VALUE   ZERO.
     03  ERR-SW              PIC  9(01)  VALUE   ZERO.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
     03  WK-ZEINUKI          PIC S9(10)  VALUE   ZERO.
     03  WK-GENKAK           PIC S9(10)V9(01)  VALUE   ZERO.
     03  WK-GENKAN           PIC S9(10)  VALUE   ZERO.
     03  WK-ZEI              PIC  9(03)  VALUE   ZERO.
     03  IX                  PIC  9(01)  VALUE   ZERO.
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
*明細単位消費税計算
 01  TAX-MEIS.
     03  TAX-MEIS-R          OCCURS  6.
         05  TAX-FUGO        PIC  X(01).
         05  TAX-KINGAKU     PIC S9(10).
         05  MEIS-KINGAKU    PIC S9(10).
     03  TAX-GK-KINGAKU      PIC S9(10).
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
     03  PRT-ERR             PIC  N(10) VALUE
         NC"プリンター　　エラー".
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
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
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
*----<< 改ページ制御 >--*
     MOVE      99                 TO   LINE-CNT.
*
*----<< 変数クリア >--*
     MOVE      ZERO               TO   PAGE-CNT.
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
     OPEN      OUTPUT  PRTFILE.
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
     CLOSE       PRTFILE.
*
 END-EXIT.
     EXIT.
**********************************************************
*                    集計処理
**********************************************************
 SYUKEI-SEC                   SECTION.
*----<< 明細原価金額セット >>--*
*    原価金額算出
     COMPUTE WK-GENKAK = SIH-F113 / 10.
     IF  SIH-F112  =  "-"
         COMPUTE  WK-GENKAK  =  WK-GENKAK  *  -1
     END-IF.
     MOVE      WK-GENKAK          TO        PRT-GENKAK.
*----<< 明細単位税抜き金額算出 >>--*
     INITIALIZE                             TAX-MEIS.
     MOVE  SIH-F203               TO        TAX-FUGO(1).
     COMPUTE  TAX-KINGAKU(1) ROUNDED = SIH-F204 / 10.
     MOVE  SIH-F206               TO        TAX-FUGO(2).
     COMPUTE  TAX-KINGAKU(2) ROUNDED = SIH-F207 / 10.
     MOVE  SIH-F209               TO        TAX-FUGO(3).
     COMPUTE  TAX-KINGAKU(3) ROUNDED = SIH-F210 / 10.
     MOVE  SIH-F212               TO        TAX-FUGO(4).
     COMPUTE  TAX-KINGAKU(4) ROUNDED = SIH-F213 / 10.
     MOVE  SIH-F215               TO        TAX-FUGO(5).
     COMPUTE  TAX-KINGAKU(5) ROUNDED = SIH-F216 / 10.
     MOVE  SIH-F218               TO        TAX-FUGO(6).
     COMPUTE  TAX-KINGAKU(6) ROUNDED = SIH-F219 / 10.
*    消費税抜き計算
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX  >  6
      IF  TAX-KINGAKU(IX) >  ZERO
          IF  SIH-F112  =  "-"
              COMPUTE TAX-KINGAKU(IX) = TAX-KINGAKU(IX) * -1
          END-IF
*         消費税抜き算出
          IF  JWW-F06 <= SIH-F06
              COMPUTE TAX-KINGAKU(IX) ROUNDED =
                      TAX-KINGAKU(IX) / JWW-F07
              COMPUTE WK-ZEI = JWW-F08 * 100
          ELSE
              COMPUTE TAX-KINGAKU(IX) ROUNDED =
                      TAX-KINGAKU(IX) / JWW-F04
              COMPUTE WK-ZEI = JWW-F05 * 100
          END-IF
          ADD  TAX-KINGAKU(IX)    TO    TAX-GK-KINGAKU
******DISPLAY "TAX-KINGAKU = " TAX-KINGAKU(IX) UPON CONS
******DISPLAY "IX          = " IX          UPON CONS
      END-IF
     END-PERFORM.
*----<< 原価金額（消費税抜き）セット >>--*
     MOVE    TAX-GK-KINGAKU       TO        PRT-GENKAN.
     MOVE    WK-ZEI               TO        PRT-ZEI.
*
*店舗計／総合計に加算
     ADD       WK-GENKAK          TO        GENKEIK.
     ADD       WK-GENKAK          TO        SOGKEIK.
     ADD       TAX-GK-KINGAKU     TO        GENKEIN.
     ADD       TAX-GK-KINGAKU     TO        SOGKEIN.
*
 SYUKEI-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*
*----<< クリア >>--*
     MOVE      SPACE              TO        PRT-HEADX.
*
*----<< システム日付時刻 >>--*
     MOVE      SYS-DATE(1:4)      TO        PRT-SYSYY.
     MOVE      WK-M               TO        PRT-SYSMM.
     MOVE      WK-D               TO        PRT-SYSDD.
     MOVE      WK-TIME(1:2)       TO        PRT-SYSHH.
     MOVE      WK-TIME(3:2)       TO        PRT-SYSMN.
     MOVE      WK-TIME(5:2)       TO        PRT-SYSSS.
*
     ADD       1                  TO        PAGE-CNT.
     MOVE      PAGE-CNT           TO        PRT-LPAGE.
*----<< 項目セット >>--*
*支払締日セット
     MOVE      SIH-F01(1:4)       TO        SIME-YYYY.
     MOVE      SIH-F01(5:2)       TO        SIME-MM.
     MOVE      SIMEYMD            TO        PRT-SIMEDT.
*取引先ＣＤセット
     MOVE      SIH-F02            TO        PRT-TORICD.
*取引先名称取得
     MOVE      SIH-F02            TO        TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = SPACE
         MOVE  TOK-F03            TO        PRT-TORINM
     ELSE
         MOVE  ALL NC"＊"         TO        PRT-TORINM
     END-IF.
*
**** IF  PAGE-CNT  >  1
         MOVE      "HEAD1"        TO        PRT-GRP.
         MOVE      "FSY75202"     TO        PRT-FMT.
         MOVE      "PW"           TO        PRT-PRO.
         MOVE      "A001"         TO        PRT-CTL.
         WRITE     PRT-FSY75202.
**** END-IF.
*
     MOVE      9                  TO        LINE-CNT.
     MOVE      0                  TO        HIT-FLG.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< クリア >>--*
     MOVE      SPACE              TO        PRT-MEISX.
*
*----<< 店舗ＣＤ／店舗名 >>--*
     IF  HIT-FLG  =  0
         MOVE  SIH-F02            TO        TEN-F52
         MOVE  SIH-F03            TO        PRT-TENCD  TEN-F011
         PERFORM  HTENMS-READ-SEC
         IF  HTENMS-INV-FLG = SPACE
             MOVE  TEN-F03        TO        PRT-TENNM
         ELSE
             MOVE  ALL NC"＊"     TO        PRT-TENNM
         END-IF
         MOVE  1                  TO        HIT-FLG
     END-IF.
*検収日
     MOVE  SIH-F06(1:4)           TO        KENSYUBI-NEN.
     MOVE  SIH-F06(5:2)           TO        KENSYUBI-TSUKI.
     MOVE  SIH-F06(7:2)           TO        KENSYUBI-HI.
     MOVE  KENSYUBI               TO        PRT-KENSBI.
*伝票番号
     MOVE  SIH-F04                TO        PRT-NDENNO.
*部門
     MOVE  SIH-F109               TO        PRT-BUMON.
*伝票区分
     MOVE  SIH-F110               TO        PRT-DENKU.
*伝票区分名
     MOVE  ":"                    TO        PRT-KUGIR1.
     MOVE  ZERO                   TO        MES-F01.
     MOVE  "DCM002"               TO        MES-F02.
     MOVE  SIH-F110               TO        MES-F04.
     PERFORM EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
           MOVE  MES-F05          TO        PRT-DENKUN
     ELSE
           MOVE  ALL NC"＊"       TO        PRT-DENKUN
     END-IF.
*伝票種類
     MOVE  SIH-F111               TO        PRT-DENSYU.
*伝票種類名
     MOVE  ":"                    TO        PRT-KUGIR2.
     MOVE  ZERO                   TO        MES-F01.
     MOVE  "DCM003"               TO        MES-F02.
     MOVE  SIH-F111               TO        MES-F04.
     PERFORM EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
           MOVE  MES-F05          TO        PRT-DENSYN
     ELSE
           MOVE  ALL NC"＊"       TO        PRT-DENSYN
     END-IF.
*原価金額合計（税込）
     MOVE  WK-GENKAK              TO        PRT-GENKAK.
*****MOVE  WK-GENKAN              TO        PRT-GENKAN.
     MOVE  TAX-GK-KINGAKU         TO        PRT-GENKAN.
*税率
     MOVE  "("                    TO        PRT-KUGIR3.
     MOVE  WK-ZEI                 TO        PRT-ZEI.
     MOVE  NC"％"                 TO        PRT-ZEIN.
     MOVE  ")"                    TO        PRT-KUGIR4.
*
*----<< 明細出力 >>--*
     MOVE  "MEIS1"                TO        PRT-GRP
     MOVE  "FSY75202"             TO        PRT-FMT
     MOVE  "PW"                   TO        PRT-PRO
     MOVE  "A001"                 TO        PRT-CTL
     WRITE PRT-FSY75202.
*
     ADD   1                      TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 TENWRT-SEC                   SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      GENKEIK            TO        PRT-TENGKK.
     MOVE      GENKEIN            TO        PRT-TENGKN.
*
*----<< 出力 >>--*
     MOVE  "TENKEI"               TO        PRT-GRP
     MOVE  "FSY75202"             TO        PRT-FMT
     MOVE  "PW"                   TO        PRT-PRO
     MOVE  "A001"                 TO        PRT-CTL
     WRITE PRT-FSY75202.
*
     ADD   3                      TO        LINE-CNT
*
*----<< 集計変数クリア >>--*
     INITIALIZE                             TKEI-AREA.
     MOVE  0                      TO        HIT-FLG.
*
 TENWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 SOUWRT-SEC                   SECTION.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         MAX-LINE
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      SOGKEIK            TO        PRT-SOUGKK.
     MOVE      SOGKEIN            TO        PRT-SOUGKN.
*
*----<< 出力 >>--*
     MOVE  "SOKKEI"               TO        PRT-GRP
     MOVE  "FSY75202"             TO        PRT-FMT
     MOVE  "PW"                   TO        PRT-PRO
     MOVE  "A001"                 TO        PRT-CTL
     WRITE PRT-FSY75202.
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
