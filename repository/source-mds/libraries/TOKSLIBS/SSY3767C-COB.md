# SSY3767C

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3767C.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：受領データ取込＆変換処理           *
*    作成日／更新日　：2010/10/13                         *
*    作成者／更新者　：ＮＡＶ飯田                         *
*    処理概要　　　　：                                   *
*      受領データを受領累積ファイルに変換し累積する。     *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3767C.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/10/13.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領データ
     SELECT  NFJYUDT
       ASSIGN        TO  DA-01-S-NFJYUDT
       FILE STATUS   IS  JYD-ST.

*受領累積ファイル
     SELECT  NFNJYRF
       ASSIGN        TO  DA-01-VI-NFNJYRL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  JYR-F01 *> データ受信日
                         JYR-F02 *> 仕入計上日
                         JYR-F05 *> 店舗ＣＤ
                         JYR-F07 *> 伝票番号
                         JYR-F04 *> 赤黒区分
                         JYR-F08 *> 行番号　
       FILE  STATUS  IS  JYR-ST.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受領データ  レコード長＝ 720 BYTE
****************************************************************
 FD  NFJYUDT           BLOCK     CONTAINS  5    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  JYD-REC.
     03  JYD-DTID               PIC  X(03).
     03  JYD-KNR-RENBN          PIC  9(13).
     03  JYD-SYS-KNRHZK         PIC  9(08).
     03  JYD-DTCRE-YMD          PIC  9(08).
     03  JYD-DTCRE-HMD          PIC  9(06).
     03  JYD-HOJNCD             PIC  9(02).

     03  JYO-SI01               PIC  X(01).
     03  JYD-HOJNNM             PIC  X(20).
     03  JYO-SO01               PIC  X(01).

     03  JYD-HOJNNMK            PIC  X(15).
     03  JYD-TENCD              PIC  9(03).

     03  JYO-SI02               PIC  X(01).
     03  JYD-TENNM              PIC  N(10).
     03  JYO-SO02               PIC  X(01).

     03  JYD-TENNMK             PIC  X(15).
     03  JYD-BMNCD              PIC  9(02).

     03  JYO-SI03               PIC  X(01).
     03  JYD-BMNNM              PIC  X(20).
     03  JYO-SO03               PIC  X(01).

     03  JYD-BMNNMK             PIC  X(15).
     03  JYD-SIRCD              PIC  9(06).

     03  JYO-SI04               PIC  X(01).
     03  JYD-SIRNM              PIC  X(20).
     03  JYO-SO04               PIC  X(01).

     03  JYD-SIRNMK             PIC  X(15).
     03  JYD-SIH-SIRCD          PIC  9(06).

     03  JYO-SI05               PIC  X(01).
     03  JYD-SIH-SIRNM          PIC  X(20).
     03  JYO-SO05               PIC  X(01).

     03  JYD-SIH-SIRNMK         PIC  X(15).
     03  JYD-SYK-GYM-SIRCD      PIC  9(06).
     03  JYD-DENKBN             PIC  9(02).
     03  JYD-AKAKRKBN           PIC  9(01).
     03  JYD-DENNO              PIC  9(08).
     03  JYD-GYONO              PIC  9(02).
     03  JYD-MOT-DENNO          PIC  9(08).
     03  JYD-HASSEIBI           PIC  9(08).
     03  JYD-SIR-KEIJYBI        PIC  9(08).
     03  JYD-SYOCD-KBN          PIC  9(01).
     03  JYD-JANCD              PIC  9(13).
     03  JYD-SYOCD              PIC  9(08).
     03  JYD-OPT-SIYORAN        PIC  X(20).
     03  JYD-GTIN               PIC  X(14).

     03  JYO-SI06               PIC  X(01).
     03  JYD-HINNM              PIC  X(20).
     03  JYO-SO06               PIC  X(01).

     03  JYD-HINNMK             PIC  X(25).

     03  JYO-SI07               PIC  X(01).
     03  JYD-KIKAKNM            PIC  X(20).
     03  JYO-SO07               PIC  X(01).

     03  JYD-KIKAKNMK           PIC  X(25).
     03  JYD-SURYO              PIC  9(06).
     03  JYD-SOGK-TORKBN        PIC  9(01).
     03  JYD-GTANKA-ZNUKI       PIC  9(09).
     03  JYD-GTANKA-ZKOMI       PIC  9(09).
     03  JYD-GKINGK-ZNUKI       PIC  9(09).
     03  JYD-GKINGK-ZKOMI       PIC  9(09).
     03  JYD-ZEIKBN             PIC  9(01).
     03  JYD-ZEIRT              PIC  9(04).
     03  JYD-ZEIGK              PIC  9(09).
     03  JYD-UTANKA-ZNUKI       PIC  9(07).
     03  JYD-UTANKA-ZKOMI       PIC  9(07).
     03  JYD-TOKKA-KBN          PIC  9(01).
     03  JYD-PBKBN              PIC  9(01).
     03  JYD-GENKA-KBN          PIC  9(01).
     03  JYD-JYURY-KBN          PIC  9(02).
     03  JYD-RIYU-KBN           PIC  9(02).
     03  JYD-RIYUCD             PIC  9(02).

     03  JYO-SI08               PIC  X(01).
     03  JYD-RIYUNM             PIC  X(20).
     03  JYD-RIYUNM-N  REDEFINES JYD-RIYUNM
                                PIC  N(10).
     03  JYO-SO08               PIC  X(01).

     03  JYD-RENRK-JIKO1        PIC  X(30).
     03  JYD-RENRK-JIKO2        PIC  X(30).
     03  JYD-NOHNSK-TENCD       PIC  9(03).

     03  JYO-SI09               PIC  X(01).
     03  JYD-NOHNSK-TENNM       PIC  X(20).
     03  JYO-SO09               PIC  X(01).

     03  JYD-NOHNSK-TENNMK      PIC  X(15).
     03  JYD-CENTER-KEIY-KBN    PIC  9(01).
     03  JYD-CENTER-CD          PIC  9(05).

     03  JYO-SI10               PIC  X(01).
     03  JYD-CENTER-NM          PIC  X(20).
     03  JYO-SO10               PIC  X(01).

     03  FILLER                 PIC  X(55).

******************************************************************
*    受領累積ファイル
******************************************************************
 FD  NFNJYRF          LABEL     RECORD    IS   STANDARD.
                      COPY      NFNJYRF   OF   XFDLIB
                      JOINING   JYR       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JYD-ST             PIC  X(02).
     03  JYR-ST             PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYD-ERR            PIC N(15) VALUE
         NC"受領データエラー".
     03  JYR-ERR            PIC N(15) VALUE
         NC"受領累積ファイルエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  FG-END             PIC  X(03)  VALUE SPACE.
     03  FG-INF-END         PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-CNT             PIC  9(07)  VALUE ZERO.
     03  OT-CNT             PIC  9(07)  VALUE ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME             PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE             PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*------------------------------------------------------------*
 LINKAGE              SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-STYMD             PIC   9(08).
 01  PARA-EDYMD             PIC   9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING    PARA-JDATE
                                          PARA-STYMD
                                          PARA-EDYMD.
**************************************************************
 DECLARATIVES.
 JYD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJYUDT.
     MOVE        JYD-ST    TO        E-ST.
     MOVE        "NFJYUDT" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYD-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFNJYRF.
     MOVE        JYR-ST    TO        E-ST.
     MOVE        "NFNJYRF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  FG-END = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
* ファイルのＯＰＥＮ
     OPEN  INPUT  NFJYUDT.
     OPEN  I-O    NFNJYRF.
* パラメータ項目初期設定
     MOVE  ZERO             TO  PARA-JDATE.
     MOVE  99999999         TO  PARA-STYMD.
     MOVE  ZERO             TO  PARA-EDYMD.
* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD  TO  SYS-DATE.
     MOVE  SKYDTCKB-OUT-YMD  TO  PARA-JDATE.

     PERFORM  RD-INF-SEC.
     IF  FG-INF-END = "END"
         MOVE  "END"        TO  FG-END
         GO TO  INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  入力ファイル読込み処理                            1.1       *
****************************************************************
 RD-INF-SEC              SECTION.
     READ  NFJYUDT
       AT  END
         MOVE  "END"        TO  FG-INF-END
         GO TO  RD-INF-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

     IF  JYD-DTID = "CH2" *> ヘッダレコード
         GO TO  RD-INF-SEC
     END-IF.

     IF    JYD-SIR-KEIJYBI  <   PARA-STYMD
           MOVE   JYD-SIR-KEIJYBI   TO  PARA-STYMD
     END-IF.

     IF    JYD-SIR-KEIJYBI  >   PARA-EDYMD
           MOVE   JYD-SIR-KEIJYBI   TO  PARA-EDYMD
     END-IF.

 RD-INF-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
     PERFORM  EDWT-SEC.
     PERFORM  RD-INF-SEC.
     IF  FG-INF-END = "END"
         MOVE  "END"        TO  FG-END
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理                                     2.1
****************************************************************
 EDWT-SEC               SECTION.
*
     MOVE  "EDWT-SEC"       TO  S-NAME.
*
     MOVE  SYS-DATE         TO  JYR-F01. *> 伝票区分
     MOVE  JYD-SIR-KEIJYBI  TO  JYR-F02. *> 仕入計上日
     MOVE  JYD-TENCD        TO  JYR-F05. *> 店舗ＣＤ
     MOVE  JYD-DENNO        TO  JYR-F07. *> 伝票番号
     MOVE  JYD-AKAKRKBN     TO  JYR-F04. *> 赤黒区分
     MOVE  JYD-GYONO        TO  JYR-F08. *> 行番号
     READ  NFNJYRF
       INVALID
         CONTINUE
       NOT INVALID
         GO TO  EDWT-EXIT
     END-READ.
*レコード初期化
     MOVE  SPACE            TO  JYR-REC.
     INITIALIZE  JYR-REC.
*データ受信日
     MOVE  SYS-DATE         TO  JYR-F01.
*仕入計上日
     MOVE  JYD-SIR-KEIJYBI  TO  JYR-F02.
*伝票区分
     MOVE  JYD-DENKBN       TO  JYR-F03.
*赤黒区分
     MOVE  JYD-AKAKRKBN     TO  JYR-F04.
*店舗CD
     MOVE  JYD-TENCD        TO  JYR-F05.
*店舗名称
     MOVE  JYD-TENNMK       TO  JYR-F06.
*伝票番号
     MOVE  JYD-DENNO        TO  JYR-F07.
*行番号
     MOVE  JYD-GYONO        TO  JYR-F08.
*元伝票番号
     MOVE  JYD-MOT-DENNO    TO  JYR-F09.
*商品CD
     MOVE  JYD-SYOCD        TO  JYR-F10.
*JANCD
     MOVE  JYD-JANCD        TO  JYR-F11.
*数量
     MOVE  JYD-SURYO        TO  JYR-F12.
*****COMPUTE JYR-F12 = JYD-SURYO / 10.
*原価単価(税抜）
     MOVE  JYD-GTANKA-ZNUKI TO  JYR-F13.
*****COMPUTE JYR-F13 = JYD-GTANKA-ZNUKI / 100.
*****DISPLAY "JYD-GTANKA-ZNUKI = " JYD-GTANKA-ZNUKI UPON CONS.
*****DISPLAY "JYR-F13          = " JYR-F13          UPON CONS.
*売価単価(税抜）
     MOVE  JYD-UTANKA-ZNUKI TO  JYR-F14.
*****COMPUTE JYR-F14 = JYD-UTANKA-ZNUKI / 100.
*****DISPLAY "JYD-UTANKA-ZNUKI = " JYD-UTANKA-ZNUKI UPON CONS.
*****DISPLAY "JYR-F14          = " JYR-F14          UPON CONS.
*受領区分
     MOVE  JYD-JYURY-KBN    TO  JYR-F15.
*理由区分
     MOVE  JYD-RIYU-KBN     TO  JYR-F16.
*理由CD
     MOVE  JYD-RIYUCD       TO  JYR-F17.
*理由名
     MOVE  JYD-RIYUNM-N     TO  JYR-F18.
*店舗名漢字
     MOVE  JYD-TENNM        TO  JYR-F19.
*品名カナ
     MOVE  JYD-HINNMK       TO  JYR-F20.
*規格カナ
     MOVE  JYD-KIKAKNMK     TO  JYR-F21.
*空白
     MOVE  SPACE            TO  JYR-FIL.

     WRITE  JYR-REC.
     ADD  1  TO  OT-CNT.
 EDWT-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "NFJYUDT IN = "  IN-CNT  UPON CONS.
     DISPLAY  "NFNJYRF OT = "  OT-CNT  UPON CONS.
* ファイルのＯＰＥＮ
     CLOSE  NFJYUDT.
     CLOSE  NFNJYRF.

 END-EXIT.
     EXIT.
*****************<<  SSY3767C   END PROGRAM  >>******************

```
