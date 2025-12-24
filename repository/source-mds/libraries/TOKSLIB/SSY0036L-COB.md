# SSY0036L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0036L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　出荷明細表                        *
*    モジュール名　　　　：　出荷明細表　出力                  *
*    作成日／更新日　　　：　99/09/16                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷明細ワークを読み、出荷明細表  *
*                            を作表する。                      *
*                                                              *
*　　更新日／更新者　　　：　04/07/02  /  YOSHIDA              *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSY0036L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       99.09.16.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*出荷明細ワーク
     SELECT   SHWSYUKF       ASSIGN        TO  01-S-SHWSYUKF
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
     SELECT   PRTF           ASSIGN      TO  GS-XU04LP
                             ORGANIZATION         IS   SEQUENTIAL
                             ACCESS MODE          IS   SEQUENTIAL
                             SYMBOLIC DESTINATION IS  "PRT"
                             PROCESSING MODE      IS   PRT-PRO
                             GROUP                IS   PRT-GRP
                             FORMAT               IS   PRT-FMT
                             SELECTED FUNCTION    IS   PRT-FNC
                             UNIT     CONTROL     IS   PRT-CTL
                             FILE STATUS          IS   PRT-STA
                             DESTINATION-1        IS   PRT-DES.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*出荷明細ワーク
 FD  SHWSYUKF        BLOCK CONTAINS   5  RECORDS.
     COPY     SHWSYUKF OF  XFDLIB
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
 FD  PRTF.
     COPY     FSY00361 OF  XMDLIB
     JOINING  PRT      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*プリンタＦ制御用
 01  PRT-CONTROL.
     03  PRT-PRO           PIC  X(02).
     03  PRT-GRP           PIC  X(08).
     03  PRT-FMT           PIC  X(08).
     03  PRT-DES           PIC  X(08).
     03  PRT-CTL           PIC  X(06).
     03  PRT-FNC           PIC  X(04).
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
     03  WK-TANA             PIC  X(06)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
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
         05  FILLER          PIC  X(12)  VALUE  "### SSY0036L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSY0036L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** SHWSYUKF ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-PRINT.
         05  FILLER          PIC  X(13)  VALUE  "*** PRTF     ".
         05  FILLER          PIC  X(10)  VALUE  "  PAGE  = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
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
     USE      AFTER  EXCEPTION   PROCEDURE       SHWSYUKF.
     MOVE    "SHWSYUKF"    TO    ERR-FL-ID.
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
     DISPLAY  "**  SSY0036L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSY0036L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
     OPEN     INPUT       SHWSYUKF.
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
     MOVE     62          TO    MAX-LINE.
     MOVE     SPACE       TO    PRT-CONTROL.
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
     MOVE    SPACE               TO  PRT-HEADX.
*日付
     MOVE    WK-SYSDT-YY         TO  PRT-SYSYY.
     MOVE    WK-SYSDT-MM         TO  PRT-SYSMM.
     MOVE    WK-SYSDT-DD         TO  PRT-SYSDD.
*頁
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PRT-PAGE.
*ﾊﾞｯﾁNO.
     MOVE    SYU-F011            TO  WK-JDATE.
     MOVE    SYU-F012            TO  WK-JTIME.
     MOVE    SYU-F013            TO  WK-TORI.
     MOVE    WK-BATCH            TO  PRT-BATCH.
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
         MOVE    TOK-F02   TO        PRT-TORINM
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F02   TO        PRT-TORINM
     END-IF.
*納品日
     MOVE    WK-NOUHIN-YY        TO  PRT-NOUYY.
     MOVE    WK-NOUHIN-MM        TO  PRT-NOUMM.
     MOVE    WK-NOUHIN-DD        TO  PRT-NOUDD.
*倉庫名の設定
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT-SOKONM
           NOT INVALID  KEY
              MOVE  SOK-F02      TO  PRT-SOKONM
     END-READ.
*部門の設定
     MOVE  SYU-F04               TO  PRT-BUMON.
     IF    PARA-OUTNO   =    1
*          店舗名の設定
           MOVE  SYU-F013        TO  TEN-F52
           MOVE  SYU-F03         TO  PRT-TENCD  TEN-F011
           READ  TENMS1
              INVALID  KEY
                 MOVE  SPACE     TO  PRT-TENNM
              NOT INVALID  KEY
                 MOVE  TEN-F02   TO  PRT-TENNM
           END-READ
     END-IF.
*出力順の設定
     IF    PARA-OUTNO   =    1
           MOVE    NC"店舗＋部門＋_番"     TO   PRT-OUTNM
     ELSE
           IF      PARA-OUTNO    =     2
                   MOVE NC"部門＋_番毎集計"  TO PRT-OUTNM
           ELSE
                   MOVE SPACE               TO   PRT-OUTNM
           END-IF
     END-IF.
*_番（１：１）の設定
     MOVE  WK-TANA(1:1)         TO   PRT-TANA1.
*印刷
     MOVE "FSY00361"             TO  PRT-FMT.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "A000"                 TO  PRT-CTL.
     MOVE "HEAD"                 TO  PRT-GRP.
     WRITE PRT-FSY00361.
*
     MOVE  9                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE    "BODY-WT-SEC"     TO  S-NAME.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  PAGE-FLG   =  1
         PERFORM  HEAD-WT-SEC
         MOVE     ZERO         TO  PAGE-FLG
     END-IF.
*明細部のクリア
     MOVE  SPACE               TO  PRT-BODYX.
*_番
     MOVE  SYU-F05(2:3)        TO  PRT-TANA2.
     MOVE  SYU-F05(5:2)        TO  PRT-TANA3.
*商品名
     MOVE  SYU-F061            TO  SHO-F011.
     MOVE  SYU-F062            TO  SHO-F012.
     READ  MEIMS1
           INVALID
              MOVE  SPACE      TO  PRT-SHONM
              MOVE  SYU-F081   TO  PRT-KANANM(1:15)
              MOVE  SYU-F082   TO  PRT-KANANM(16:15)
           NOT INVALID  KEY
              MOVE  SHO-F02    TO  PRT-SHONM
              MOVE  SPACE      TO  PRT-KANANM
     END-READ.
*
*数量
     MOVE  SYU-F09             TO  PRT-SURYO.
*訂正数量
     IF    SYU-F09  NOT  =  SYU-F10
           MOVE  SYU-F10       TO  PRT-TSURYO
*    ELSE
*          MOVE  ZERO          TO  PRT-TSURYO
     END-IF.
*売単価(税込)
*    MOVE  SYU-F12             TO  PRT-URITAN.
     MOVE  SYU-F18             TO  PRT-URITAN.
*
*PARA-OUTNO = 1 （店舗毎集計）時
     IF    PARA-OUTNO   =    1
***  引当済み印表示
           IF    SYU-F17     =    1
                 MOVE   SPACE    TO  PRT-HOSHI
           ELSE
                 MOVE   NC"★"   TO  PRT-HOSHI
           END-IF
***  伝票_
           MOVE  SYU-F16     TO  PRT-DENNO
     END-IF.
*
***  売価単価（税込）
*    MOVE  SYU-F18           TO  PRT-URITNZ.
*
*印刷
     MOVE "FSY00361"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-FSY00361.
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                出荷明細ワークリード処理
*=============================================================
 SYU-RD-SEC      SECTION.
     MOVE    "SYU-RD-SEC"  TO  S-NAME.
*リード
     READ  SHWSYUKF      NEXT
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
     CLOSE      SHWSYUKF.
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

```
