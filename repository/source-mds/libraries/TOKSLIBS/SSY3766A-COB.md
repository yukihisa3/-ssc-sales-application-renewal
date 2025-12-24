# SSY3766A

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3766A.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：ＴＲＡＮＴＲＡＮ連携データ作成     *
*    作成日／更新日　：2010/10/13                         *
*    作成者／更新者　：ＮＡＶ飯田                         *
*    処理概要　　　　：                                   *
*      箱数ファイル、数量訂正ファイルよりＴＲＡＮＴＲ     *
*      ＡＮ連携データ（３種類）を作成する。               *
*      ＣＳＶ形式にて出力に変更                           *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3766A.
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

* 箱数ファイル
     SELECT  NFHAKOF
       ASSIGN         TO  NFHAKOL3
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  HK3-F01 *> 管理番号
                          HK3-F06 *> 店舗ＣＤ
                          HK3-F07 *> 納品場所
                          HK3-F08 *> 店着日
                          HK3-F05 *> 作場ＣＤ
       FILE STATUS    IS  HK3-ST.

* 数量訂正ファイル
     SELECT  NFSUTEF
       ASSIGN         TO  NFSUTEL3
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  ST3-F01 *> 管理番号
                          ST3-F07 *> 店舗ＣＤ
                          ST3-F08 *> 納品場所
                          ST3-F09 *> 店着日
                          ST3-F06 *> 作場ＣＤ
                          ST3-F05 *> 伝票番号
       FILE STATUS    IS  ST3-ST.

*基本情報ファイル
     SELECT  NFJOHOF
       ASSIGN         TO  NFJOHOL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  KH1-F01 *> 管理番号
                          KH1-F05 *> 作場ＣＤ
                          KH1-F06 *> 店舗ＣＤ
                          KH1-F07 *> 伝票番号
                          KH1-F08 *> 行番号
                          KH1-F09 *> 納品日
       FILE STATUS    IS  KH1-ST.
* 作場マスタ
     SELECT  SAKUBAF
       ASSIGN         TO  SAKUBAL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SKB-F01
       FILE STATUS    IS  SKB-ST.


**   SELECT  HSHOTBL
**     ASSIGN         TO  SHOTBL1
**     ORGANIZATION   IS  INDEXED
**     ACCESS MODE    IS  RANDOM
**     RECORD KEY     IS  SCV-F01 *> 相手取引先ＣＤ
**                        SCV-F02 *> 相手商品コード
**     FILE STATUS    IS  SCV-ST.

* 商品名称マスタ
     SELECT  NFMEIMS
       ASSIGN         TO  NFMEIMS1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SYO-F01  *> ナフコ商品コード
       FILE STATUS    IS  SYO-ST.

* ＴＲＡＮＴＲＡＮ連携データ（箱数ファイル）
     SELECT  NFHAKDT
       ASSIGN         TO  DA-01-S-NFHAKDT
       FILE STATUS    IS  HKD-ST.

* ＴＲＡＮＴＲＡＮ連携データ（数量訂正ファイル）
     SELECT  NFSUTDT
       ASSIGN         TO  DA-01-S-NFSUTDT
       FILE STATUS    IS  STD-ST.

* ＴＲＡＮＴＲＡＮ連携データ（ＦＡＸ発注一括取込ファイル）
     SELECT  NFFAXDT
       ASSIGN         TO  DA-01-S-NFFAXDT
       FILE STATUS    IS  FXD-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 箱数ファイル                                       *
****************************************************************
 FD  NFHAKOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK3       AS   PREFIX.
****************************************************************
*    FILE = 数量訂正ファイル                                   *
****************************************************************
 FD  NFSUTEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST3       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル                                   *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH1       AS   PREFIX.
****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = 商品変換ＴＢＬ                                     *
****************************************************************
** FD  HSHOTBL
**                       LABEL     RECORD    IS   STANDARD.
**                       COPY      HSHOTBL   OF   XFDLIB
**                       JOINING   SCV       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  NFMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFMEIMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = ＴＲＡＮＴＲＡＮ連携データ（箱数ファイル）         *
*           レコード長＝ 34 BYTE                               *
****************************************************************
 FD  NFHAKDT           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  HKD-REC.
     03  HKD-SIRCD              PIC  9(06).   *>仕入先CD
     03  HKD-FIL-001            PIC  X(01).
     03  HKD-SYUK-GYOM-SIRCD    PIC  9(06).   *>出荷仕入先CD
     03  HKD-FIL-002            PIC  X(01).
     03  HKD-TENCD              PIC  9(03).   *>店舗CD
     03  HKD-FIL-003            PIC  X(01).
     03  HKD-NOHN-BASY-CD       PIC  X(01).   *>納品場所CD
     03  HKD-FIL-004            PIC  X(01).
     03  HKD-HAKOSU             PIC  9(06).   *>出荷総梱数
     03  HKD-FIL-005            PIC  X(01).
     03  HKD-NYUK-YOTEI-YMD     PIC  9(08).   *>入荷予定日
     03  HKD-FIL-006            PIC  X(01).
     03  HKD-NYUK-YOTEI-TIM     PIC  9(04).   *>入荷予定時刻
     03  HKD-FIL-007            PIC  X(01).
     03  HKD-TYOKUSO-KBN        PIC  9(02).   *>入荷予定時刻

****************************************************************
*    FILE = ＴＲＡＮＴＲＡＮ連携データ（数量訂正ファイル）     *
*           レコード長＝ 45 BYTE                               *
****************************************************************
 FD  NFSUTDT           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  STD-REC.
     03  STD-SIRCD              PIC  9(06). *>仕入先CD
     03  STD-FIL-001            PIC  X(01).
     03  STD-SYUK-GYOM-SIRCD    PIC  9(06). *>出荷仕入先CD
     03  STD-FIL-002            PIC  X(01).
     03  STD-TENCD              PIC  9(03). *>発注店舗CD
     03  STD-FIL-003            PIC  X(01).
     03  STD-DENNO              PIC  9(08). *>発注伝票番号
     03  STD-FIL-004            PIC  X(01).
     03  STD-NOHN-SU            PIC  9(06). *>納品数
     03  STD-FIL-005            PIC  X(01).
     03  STD-NYUK-YOTEI-YMD     PIC  9(08). *>入荷予定日
     03  STD-FIL-006            PIC  X(01).
     03  STD-NYUK-YOTEI-TIM     PIC  9(04). *>入荷予定時刻
     03  STD-FIL-007            PIC  X(01).
     03  STD-TEISEI-RIY-KBN     PIC  9(02). *>訂正区分
     03  STD-FIL-008            PIC  X(01).
     03  STD-TYOKS-MOT-CD       PIC  9(02). *>直送元CD

****************************************************************
*    FILE = TRANTRAN連携ﾃﾞｰﾀ（ＦＡＸ発注一括取込ファイル）
*           レコード長＝ 156 BYTE                              *
****************************************************************
 FD  NFFAXDT           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  FXD-REC.
     03  FXD-DENNO              PIC  9(08). *>伝票番号
     03  FXD-FIL-001            PIC  X(01).
     03  FXD-BMNCD              PIC  9(02). *>部門CD
     03  FXD-FIL-002            PIC  X(01).
     03  FXD-SIRCD              PIC  9(06). *>仕入先CD
     03  FXD-FIL-003            PIC  X(01).
     03  FXD-SYUK-GYOM-SIRCD    PIC  9(06). *>出荷仕入先CD
     03  FXD-FIL-004            PIC  X(01).
     03  FXD-HACH-YMD           PIC  9(08). *>発注日
     03  FXD-FIL-005            PIC  X(01).
     03  FXD-NOHN-YOTEI-YMD     PIC  9(08). *>納品予定日
     03  FXD-FIL-006            PIC  X(01).
     03  FXD-NYUK-YOTEI-YMD     PIC  9(08). *>入荷予定日
     03  FXD-FIL-007            PIC  X(01).
     03  FXD-SYUK-YMD           PIC  9(08). *>出荷日
     03  FXD-FIL-008            PIC  X(01).
     03  FXD-HACH-KEITAI-KBN    PIC  9(02). *>発注形態区分
     03  FXD-FIL-009            PIC  X(01).
     03  FXD-TYOKSO-KBN         PIC  9(01). *>直送区分
     03  FXD-FIL-010            PIC  X(01).
     03  FXD-SYOCD              PIC  9(08). *>商品CD
     03  FXD-FIL-011            PIC  X(01).

     03  FXD-SI01               PIC  X(01). *>28
     03  FXD-HINMN              PIC  N(10). *>品名（漢字）
     03  FXD-SO01               PIC  X(01). *>29
     03  FXD-FIL-012            PIC  X(01).

     03  FXD-SI02               PIC  X(01). *>28
     03  FXD-KIKAK-NM           PIC  N(10). *>規格名（漢字）
     03  FXD-SO02               PIC  X(01). *>29
     03  FXD-FIL-013            PIC  X(01).

     03  FXD-HACH-SU            PIC  9(06). *>発注数
     03  FXD-FIL-014            PIC  X(01).
     03  FXD-NOHN-SU            PIC  9(06). *>納品数
     03  FXD-FIL-015            PIC  X(01).
     03  FXD-TEISEI-RIY-KBN     PIC  9(02). *>訂正理由区分
     03  FXD-FIL-016            PIC  X(01).
     03  FXD-GTANKA-ZNUKI       PIC  9(09). *>原単価
     03  FXD-FIL-017            PIC  X(01).
     03  FXD-UTANKA-ZNUKI       PIC  9(07). *>売単価
     03  FXD-FIL-018            PIC  X(01).
     03  FXD-NOHNSK-TENCD       PIC  9(03). *>納品店舗CD
     03  FXD-FIL-019            PIC  X(01).
     03  FXD-NOHN-BASYCD        PIC  X(01). *>納品場所
     03  FXD-FIL-020            PIC  X(01).
     03  FXD-JANCD              PIC  9(13). *>JANCD
     03  FXD-FIL-021            PIC  X(01).
     03  FXD-TYOKUSO-KBN        PIC  9(02). *>直送元コード

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  HK3-ST             PIC  X(02).
     03  ST3-ST             PIC  X(02).
     03  KH1-ST             PIC  X(02).
     03  SKB-ST             PIC  X(02).
     03  SCV-ST             PIC  X(02).
     03  SYO-ST             PIC  X(02).
     03  HKD-ST             PIC  X(02).
     03  STD-ST             PIC  X(02).
     03  FXD-ST             PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYD-ERR            PIC N(15) VALUE
         NC"受領データエラー".
     03  JYR-ERR            PIC N(15) VALUE
         NC"受領累積ファイルエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  FG-END             PIC  X(03)  VALUE SPACE.
     03  FG-NFHAKOF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFSUTEF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFJOHOF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-SAKUBAF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-HSHOTBL-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFMEIMS-INV     PIC  9(01)  VALUE ZERO.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-CNT             PIC  9(07)  VALUE ZERO.
     03  IN-CNT2            PIC  9(07)  VALUE ZERO.
     03  OT-CNT             PIC  9(07)  VALUE ZERO.
     03  OT-CNT2            PIC  9(07)  VALUE ZERO.
     03  OT-CNT3            PIC  9(07)  VALUE ZERO.

 01  KY-HK3.
     03  KY-HK3-F01         PIC  9(08). *> 管理番号
     03  KY-HK3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-HK3-F07         PIC  9(01). *> 納品場所
     03  KY-HK3-F08         PIC  9(08). *> 店着日
     03  KY-HK3-F05         PIC  9(02). *> 作場ＣＤ

 01  KY-ST3.
     03  KY-ST3-F01         PIC  9(08). *> 管理番号
     03  KY-ST3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-ST3-F07         PIC  9(01). *> 納品場所
     03  KY-ST3-F08         PIC  9(08). *> 店着日
     03  KY-ST3-F05         PIC  9(02). *> 作場ＣＤ

***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
***  エラーファイル
 01  FILE-ERR.
     03  HK3-ERR           PIC N(20) VALUE
         NC"箱数ファイル１エラー".
     03  ST3-ERR           PIC N(20) VALUE
         NC"数量訂正ファイル１エラー".
     03  KH1-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル１エラー".
     03  SKB-ERR           PIC N(20) VALUE
         NC"作場マスタラー".
     03  SCV-ERR           PIC N(20) VALUE
         NC"商品変換ＴＢＬエラー".
     03  SYO-ERR           PIC N(20) VALUE
         NC"商品名称マスタエラー".
     03  HKD-ERR           PIC N(20) VALUE
         NC"ＴＲＡＮＴＲＡＮ連携ＤＴ箱数エラー".
     03  STD-ERR           PIC N(20) VALUE
         NC"ＴＲＡＮＴＲＡＮ連携ＤＴ数量訂正エラー".
     03  FXD-ERR           PIC N(20) VALUE
         NC"ＴＲＡＮＴＲＡＮ連携ＤＴＦＡＸ発注エラー".

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

     COPY  NFHAKOF OF XFDLIB
           JOINING WHK3  AS PREFIX.
 01  WK-RD-KANRNO           PIC  9(08).
 01  IX                     PIC  9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
* 入力パラメータ
 01  PA-TANCD               PIC  9(02).
 01  PA-KBN                 PIC  9(01).
 01  PA-BACH-YMD            PIC  9(08).
 01  PA-BACH-TIM            PIC  9(04).
 01  PA-BACH-TOR            PIC  9(08).
 01  PA-KANRNO              PIC  9(08).
 01  PA-SAKBCD              PIC  X(02).
 01  PA-SYKYMD              PIC  9(08).
 01  PA-TENYMD              PIC  9(08).
*
**************************************************************
 PROCEDURE             DIVISION  USING PA-TANCD
                                       PA-KBN
                                       PA-BACH-YMD
                                       PA-BACH-TIM
                                       PA-BACH-TOR
                                       PA-KANRNO
                                       PA-SAKBCD
                                       PA-SYKYMD
                                       PA-TENYMD.
**************************************************************
 DECLARATIVES.
 HK3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HK3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ST3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     ST3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     KH1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
** SCV-ERR                   SECTION.
**     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
**     DISPLAY     SCV-ERR   UPON      CONS.
**     DISPLAY     SEC-NAME  UPON      CONS.
**     DISPLAY     SCV-ST    UPON      CONS.
**     MOVE        "4000"    TO        PROGRAM-STATUS.
**     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFMEIMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HKD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKDT.
     DISPLAY     HKD-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HKD-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTDT.
     DISPLAY     STD-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     STD-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FXD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFFAXDT.
     DISPLAY     FXD-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FXD-ST    UPON      CONS.
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
     OPEN  INPUT  NFHAKOF.
     OPEN  INPUT  NFSUTEF.
     OPEN  INPUT  NFJOHOF.
     OPEN  INPUT  SAKUBAF.
**     OPEN  INPUT  HSHOTBL.
     OPEN  INPUT  NFMEIMS.
     OPEN  OUTPUT NFHAKDT.
     OPEN  OUTPUT NFSUTDT.
     OPEN  OUTPUT NFFAXDT.
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
     MOVE  SKYDTCKB-OUT-YMD TO  SYS-DATE.

     MOVE  LOW-VALUE        TO  FG-NFSUTEF-END.
     MOVE  PA-KANRNO        TO  WK-RD-KANRNO.
     MOVE  PA-KANRNO        TO  ST3-F01.  *> 管理番号
     MOVE  ZERO             TO  ST3-F06.  *> 店舗ＣＤ
     MOVE  SPACE            TO  ST3-F07.  *> 納品場所
     MOVE  ZERO             TO  ST3-F08.  *> 店着日
     MOVE  SPACE            TO  ST3-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  ST3-F09.  *> 伝票番号
     PERFORM  RD-NFSUTEF-SEC.
     IF  FG-NFSUTEF-END = "END"
         MOVE  "END"     TO  FG-END
         GO TO  INIT-EXIT
     END-IF.

     MOVE  LOW-VALUE        TO  FG-NFHAKOF-END.
     MOVE  PA-KANRNO        TO  HK3-F01. *> 管理番号
     MOVE  ZERO             TO  HK3-F06. *> 店舗ＣＤ
     MOVE  ZERO             TO  HK3-F07. *> 納品場所
     MOVE  ZERO             TO  HK3-F08. *> 店着日
     MOVE  SPACE            TO  HK3-F05. *> 作場ＣＤ
     PERFORM  RD-NFHAKOF-SEC.

 INIT-EXIT.
     EXIT.
****************************************************************
*  入力ファイル２読込み処理                                    *
****************************************************************
 RD-NFSUTEF-SEC             SECTION.
*
     IF  FG-NFSUTEF-END = LOW-VALUE
         START  NFSUTEF  KEY >=  ST3-F01
                                 ST3-F07
                                 ST3-F08
                                 ST3-F09
                                 ST3-F06
                                 ST3-F05
           INVALID KEY
              MOVE  "END"       TO  FG-NFSUTEF-END
              MOVE  HIGH-VALUE  TO  KY-ST3
              GO TO  RD-NFSUTEF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFSUTEF-END

     END-IF.
*
     READ  NFSUTEF
       AT  END
         MOVE  "END"        TO  FG-NFSUTEF-END
         MOVE  HIGH-VALUE   TO  KY-ST3
         GO TO  RD-NFSUTEF-EXIT
     END-READ.
*管理番号チェック
     IF  ST3-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFSUTEF-END
         MOVE  HIGH-VALUE   TO  KY-ST3
         GO TO  RD-NFSUTEF-EXIT
     END-IF.
*作場ＣＤチェック
     IF  PA-SAKBCD = SPACE
         CONTINUE
     ELSE
         IF  ST3-F05 NOT = PA-SAKBCD
             GO TO  RD-NFSUTEF-SEC
         END-IF
     END-IF.
*店着日チェック
     IF  PA-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  ST3-F08 NOT = PA-TENYMD
             GO TO  RD-NFSUTEF-SEC
         END-IF
     END-IF.
*出荷日チェック
     IF  PA-SYKYMD = ZERO
         CONTINUE
     ELSE
         IF  ST3-F14 NOT = PA-SYKYMD
             GO TO  RD-NFSUTEF-SEC
         END-IF
     END-IF.
* マッチングキーの編集
     MOVE  ST3-F01          TO  KY-ST3-F01.
     MOVE  ST3-F06          TO  KY-ST3-F06.
     MOVE  ST3-F07          TO  KY-ST3-F07.
     MOVE  ST3-F08          TO  KY-ST3-F08.
     MOVE  ST3-F05          TO  KY-ST3-F05.

     ADD  1   TO  IN-CNT2.

 RD-NFSUTEF-EXIT.
     EXIT.
****************************************************************
*  入力ファイル読込み処理                                      *
****************************************************************
 RD-NFHAKOF-SEC              SECTION.
*
     IF  FG-NFHAKOF-END = LOW-VALUE
         START  NFHAKOF  KEY >=  HK3-F01
                                 HK3-F06
                                 HK3-F07
                                 HK3-F08
                                 HK3-F05
           INVALID KEY
             MOVE  "END"       TO  FG-NFHAKOF-END
             MOVE  HIGH-VALUE  TO  KY-HK3
             GO TO  RD-NFHAKOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF-END

     END-IF.
*
     READ  NFHAKOF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF-END
         MOVE  HIGH-VALUE   TO  KY-HK3
         GO TO  RD-NFHAKOF-EXIT
     END-READ.
*管理番号
     IF  HK3-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFHAKOF-END
         MOVE  HIGH-VALUE   TO  KY-HK3
         GO TO  RD-NFHAKOF-EXIT
     END-IF.
*店着日
     IF     PA-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK3-F08 NOT = PA-TENYMD
             GO TO  RD-NFHAKOF-SEC
         END-IF
     END-IF.
*作場ＣＤ
     IF     PA-SAKBCD = SPACE
         CONTINUE
     ELSE
         IF  HK3-F05 NOT = PA-SAKBCD
             GO TO  RD-NFHAKOF-SEC
         END-IF
     END-IF.
* マッチングキーの編集
     MOVE  HK3-F01          TO  KY-HK3-F01.
     MOVE  HK3-F06          TO  KY-HK3-F06.
     MOVE  HK3-F07          TO  KY-HK3-F07.
     MOVE  HK3-F08          TO  KY-HK3-F08.
     MOVE  HK3-F05          TO  KY-HK3-F05.

     ADD  1   TO  IN-CNT.

 RD-NFHAKOF-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  TRUE
       WHEN  KY-HK3 < KY-ST3
                       *> "＜"：箱数有り、数量訂正Ｆ無し
         PERFORM  UNTIL FG-NFHAKOF-END = "END"
                     OR KY-HK3 >= KY-ST3
           PERFORM  RD-NFHAKOF-SEC

         END-PERFORM

       WHEN  KY-HK3 = KY-ST3
                       *> "＝"：マッチングしたの場合

*      ＴＲＡＮＴＲＡＮ連携データ（箱数ファイル）
         MOVE  HK3-REC      TO  WHK3-REC
         PERFORM  EDWT-NFHAKDT-SEC

         PERFORM  UNTIL FG-NFSUTEF-END = "END"
                     OR KY-ST3 > KY-HK3
           PERFORM  EDWT-SEC
           PERFORM  RD-NFSUTEF-SEC
         END-PERFORM

         PERFORM  RD-NFHAKOF-SEC

       WHEN  KY-HK3 > KY-ST3
                       *> "＞"：箱数無し、数量訂正Ｆ有り
        DISPLAY "SSY3766A NFHAKOF RECORD NOT FOUND KEY="
          KY-ST3 "*"  UPON CONS
          MOVE  "4000"      TO  PROGRAM-STATUS
          STOP RUN
**         PERFORM  UNTIL FG-NFSUTEF-END = "END"
**                     OR KY-ST3 >= KY-HK3
**
**           INITIALIZE  WHK3-REC
**           MOVE  ST3-F06    TO  WHK3-F06  *> 店舗
**           MOVE  ST3-F07    TO  WHK3-F07  *> 納品場所
**           MOVE  ZERO       TO  WHK3-F09  *> 箱数
**           MOVE  ST3-F13    TO  WHK3-F08  *> 入荷予定日
**           PERFORM  EDWT-SEC
**           PERFORM  RD-NFSUTEF-SEC
**         END-PERFORM

     END-EVALUATE.

     IF  FG-NFSUTEF-END = "END"
         MOVE  "END"        TO  FG-END
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理（箱数）
****************************************************************
 EDWT-NFHAKDT-SEC               SECTION.
     MOVE  "EDWT-NFHAKDT-SEC"  TO  S-NAME.
*レコード初期化
     MOVE  SPACE            TO  HKD-REC.
     INITIALIZE  HKD-REC.
*仕入先
*****MOVE  137607           TO  HKD-SIRCD.
     MOVE  999977           TO  HKD-SIRCD.
*出荷仕入先
     MOVE  137607           TO  HKD-SYUK-GYOM-SIRCD.
*店舗CD
     MOVE  WHK3-F06         TO  HKD-TENCD.
*納品場所CD
     MOVE  WHK3-F07         TO  HKD-NOHN-BASY-CD.
*箱数
     MOVE  WHK3-F09         TO  HKD-HAKOSU.
*入荷予定日
     MOVE  WHK3-F08         TO  HKD-NYUK-YOTEI-YMD.
*入荷時刻
     MOVE  ZERO             TO  HKD-NYUK-YOTEI-TIM.
*作場マスタ索引（直送先CD取得）
     MOVE  WHK3-F05         TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC
     IF  FG-SAKUBAF-INV = 1
         MOVE 1             TO  HKD-TYOKUSO-KBN
     ELSE
         MOVE SKB-F03       TO  STD-TYOKS-MOT-CD *> 直送ＣＤ
         EVALUATE SKB-F03
             WHEN "1"   MOVE 1  TO  HKD-TYOKUSO-KBN
             WHEN "2"   MOVE 2  TO  HKD-TYOKUSO-KBN
             WHEN "3"   MOVE 3  TO  HKD-TYOKUSO-KBN
             WHEN "4"   MOVE 4  TO  HKD-TYOKUSO-KBN
             WHEN "5"   MOVE 5  TO  HKD-TYOKUSO-KBN
             WHEN "6"   MOVE 6  TO  HKD-TYOKUSO-KBN
             WHEN "7"   MOVE 7  TO  HKD-TYOKUSO-KBN
             WHEN "8"   MOVE 8  TO  HKD-TYOKUSO-KBN
             WHEN "9"   MOVE 9  TO  HKD-TYOKUSO-KBN
             WHEN "A"   MOVE 10 TO  HKD-TYOKUSO-KBN
             WHEN "B"   MOVE 11 TO  HKD-TYOKUSO-KBN
             WHEN "C"   MOVE 12 TO  HKD-TYOKUSO-KBN
             WHEN "D"   MOVE 13 TO  HKD-TYOKUSO-KBN
             WHEN "E"   MOVE 14 TO  HKD-TYOKUSO-KBN
             WHEN "F"   MOVE 15 TO  HKD-TYOKUSO-KBN
             WHEN "G"   MOVE 16 TO  HKD-TYOKUSO-KBN
             WHEN "H"   MOVE 17 TO  HKD-TYOKUSO-KBN
             WHEN "I"   MOVE 18 TO  HKD-TYOKUSO-KBN
             WHEN "J"   MOVE 19 TO  HKD-TYOKUSO-KBN
             WHEN "K"   MOVE 20 TO  HKD-TYOKUSO-KBN
             WHEN "L"   MOVE 21 TO  HKD-TYOKUSO-KBN
             WHEN "M"   MOVE 22 TO  HKD-TYOKUSO-KBN
             WHEN "N"   MOVE 23 TO  HKD-TYOKUSO-KBN
             WHEN "O"   MOVE 24 TO  HKD-TYOKUSO-KBN
             WHEN "P"   MOVE 25 TO  HKD-TYOKUSO-KBN
             WHEN "Q"   MOVE 26 TO  HKD-TYOKUSO-KBN
             WHEN "R"   MOVE 27 TO  HKD-TYOKUSO-KBN
             WHEN "S"   MOVE 28 TO  HKD-TYOKUSO-KBN
             WHEN "T"   MOVE 29 TO  HKD-TYOKUSO-KBN
             WHEN "U"   MOVE 30 TO  HKD-TYOKUSO-KBN
             WHEN "V"   MOVE 31 TO  HKD-TYOKUSO-KBN
             WHEN "W"   MOVE 32 TO  HKD-TYOKUSO-KBN
             WHEN "X"   MOVE 33 TO  HKD-TYOKUSO-KBN
             WHEN "Y"   MOVE 34 TO  HKD-TYOKUSO-KBN
             WHEN "Z"   MOVE 35 TO  HKD-TYOKUSO-KBN
             WHEN OTHER MOVE 1  TO  HKD-TYOKUSO-KBN
         END-EVALUATE
     END-IF.
*カンマセット
     MOVE  ","              TO  HKD-FIL-001  HKD-FIL-002.
     MOVE  ","              TO  HKD-FIL-003  HKD-FIL-004.
     MOVE  ","              TO  HKD-FIL-005  HKD-FIL-006.
     MOVE  ","              TO  HKD-FIL-007.
*箱数ファイル出力
     WRITE  HKD-REC.
     ADD  1  TO  OT-CNT.

 EDWT-NFHAKDT-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理
****************************************************************
 EDWT-SEC               SECTION.
     MOVE  "EDWT-SEC"       TO  S-NAME.
     IF  PA-KBN = 1  *> オンライン
*      ＴＲＡＮＴＲＡＮ連携データ（数量訂正ファイル）
         PERFORM  EDWT-NFSUTDT-SEC
     ELSE             *> 手書き
*      ＴＲＡＮＴＲＡＮ連携データ（ＦＡＸ発注一括取込ファイル）
         PERFORM  EDWT-NFFAXDT-SEC
     END-IF.

 EDWT-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理（数量訂正）
****************************************************************
 EDWT-NFSUTDT-SEC               SECTION.
*
     MOVE  "EDWT-NFSUTDT-SEC"  TO  S-NAME.
*レコード初期化
     MOVE  SPACE            TO  STD-REC.
     INITIALIZE  STD-REC.
*仕入先
*****MOVE  137607           TO  STD-SIRCD.
     MOVE  999977           TO  STD-SIRCD.
*出荷仕入先
     MOVE  137607           TO  STD-SYUK-GYOM-SIRCD.
*店舗CD
     MOVE  ST3-F06          TO  STD-TENCD.
*伝票番号
     MOVE  ST3-F09          TO  STD-DENNO.
*納品数（１０倍）
*****MOVE  ST3-F11          TO  STD-NOHN-SU.
     COMPUTE STD-NOHN-SU = ST3-F11 * 10.
*入荷予定日
     MOVE  ST3-F08          TO  STD-NYUK-YOTEI-YMD.
*入荷時刻
     MOVE  ZERO             TO  STD-NYUK-YOTEI-TIM.
*訂正区分
*****MOVE  ST3-F12          TO  STD-TEISEI-RIY-KBN.
     IF    ST3-F12 = SPACE
           MOVE "00"        TO  STD-TEISEI-RIY-KBN
     ELSE
           MOVE  ST3-F12    TO  STD-TEISEI-RIY-KBN
     END-IF.
*作場マスタ索引（直送先CD取得）
     MOVE  ST3-F05          TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC
     IF  FG-SAKUBAF-INV = 1
         MOVE ZERO          TO  STD-TYOKS-MOT-CD
     ELSE
         MOVE SKB-F03       TO  STD-TYOKS-MOT-CD *> 直送ＣＤ
     END-IF.
*甘木の場合は、直送区分を０にする。
     IF  ST3-F05  = "E1"
         MOVE ZERO          TO  STD-TYOKS-MOT-CD
     END-IF.
*カンマセット
     MOVE  ","              TO  STD-FIL-001  STD-FIL-002.
     MOVE  ","              TO  STD-FIL-003  STD-FIL-004.
     MOVE  ","              TO  STD-FIL-005  STD-FIL-006.
     MOVE  ","              TO  STD-FIL-007  STD-FIL-008.
*数量訂正ファイル出力
     WRITE  STD-REC.
     ADD  1  TO  OT-CNT2.
*
 EDWT-NFSUTDT-EXIT.
     EXIT.
****************************************************************
*    作場マスタ検索                                            *
****************************************************************
 RD-SAKUBAF-SEC          SECTION.
*
     READ  SAKUBAF
       INVALID
         MOVE  1                 TO  FG-SAKUBAF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-SAKUBAF-INV
     END-READ.
*
 RD-SAKUBAF-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理（ＦＡＸ発注）
****************************************************************
 EDWT-NFFAXDT-SEC       SECTION.
*
     MOVE  "EDWT-NFFAXDT-SEC"  TO  S-NAME.
*  基本情報マスタ
     MOVE  ST3-F01          TO  KH1-F01. *> 管理番号
     MOVE  ST3-F05          TO  KH1-F05. *> 作場ＣＤ
     MOVE  ST3-F06          TO  KH1-F06. *> 店舗ＣＤ
     MOVE  ST3-F09          TO  KH1-F07. *> 伝票番号
     MOVE  01               TO  KH1-F08. *> 行番号
     MOVE  ST3-F13          TO  KH1-F09. *> 納品日
     PERFORM  RD-NFJOHOF-SEC.
     IF  FG-NFJOHOF-INV = 1
         DISPLAY "SSY3766A NFJOHOF RECORD NOT FOUND KEY="
                 ST3-F01
             "," ST3-F06
             "," ST3-F07
             "," ST3-F08
             "," ST3-F05
             "*"  UPON CONS
         DISPLAY "         NFJOHOF KEY="
                 KH1-F01
             "," KH1-F05
             "," KH1-F06
             "," KH1-F07
             "," KH1-F08
             "," KH1-F09
             "*"  UPON CONS
               MOVE  "4000"     TO  PROGRAM-STATUS
               EXIT PROGRAM
     END-IF.
*  ナフコ商品名称マスタ
     MOVE  KH1-F13          TO  SYO-F01 *> 相手商品ＣＤ
     PERFORM  RD-NFMEIMS-SEC
*  作場マスタ
*****MOVE  WHK3-F05         TO  SKB-F01.
     MOVE  ST3-F05          TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC
*レコード初期化
     MOVE  SPACE            TO  FXD-REC.
     INITIALIZE  FXD-REC.
*伝票番号
     MOVE  ST3-F09          TO  FXD-DENNO.
*部門CD
*****MOVE  KH1-HE09         TO  FXD-BMNCD.
     MOVE  KH1-HE10         TO  FXD-BMNCD.
*仕入先
*****MOVE  137607           TO  FXD-SIRCD.
     MOVE  999977           TO  FXD-SIRCD.
*出荷仕入先
     MOVE  137607           TO  FXD-SYUK-GYOM-SIRCD.
*発注日
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  KH1-HE11         TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  FXD-HACH-YMD.
*納品予定日
*****MOVE  WHK3-F08         TO  FXD-NOHN-YOTEI-YMD.
     MOVE  ST3-F13          TO  FXD-NOHN-YOTEI-YMD.
*入荷予定日
     MOVE  ST3-F08          TO  FXD-NYUK-YOTEI-YMD.
*出荷日
     MOVE  ST3-F14          TO  FXD-SYUK-YMD.
*発注形態区分
*****MOVE  ST3-F15          TO  FXD-HACH-KEITAI-KBN.
     MOVE  ST3-FIL(1:2)     TO  FXD-HACH-KEITAI-KBN.
*直送区分
     IF  FG-SAKUBAF-INV = 1
         MOVE ZERO          TO  FXD-TYOKSO-KBN
         MOVE 1             TO  FXD-TYOKUSO-KBN
     ELSE
         MOVE 1             TO  FXD-TYOKSO-KBN *> 直送ＣＤ
         EVALUATE SKB-F03
             WHEN "1"   MOVE 1  TO  FXD-TYOKUSO-KBN
             WHEN "2"   MOVE 2  TO  FXD-TYOKUSO-KBN
             WHEN "3"   MOVE 3  TO  FXD-TYOKUSO-KBN
             WHEN "4"   MOVE 4  TO  FXD-TYOKUSO-KBN
             WHEN "5"   MOVE 5  TO  FXD-TYOKUSO-KBN
             WHEN "6"   MOVE 6  TO  FXD-TYOKUSO-KBN
             WHEN "7"   MOVE 7  TO  FXD-TYOKUSO-KBN
             WHEN "8"   MOVE 8  TO  FXD-TYOKUSO-KBN
             WHEN "9"   MOVE 9  TO  FXD-TYOKUSO-KBN
             WHEN "A"   MOVE 10 TO  FXD-TYOKUSO-KBN
             WHEN "B"   MOVE 11 TO  FXD-TYOKUSO-KBN
             WHEN "C"   MOVE 12 TO  FXD-TYOKUSO-KBN
             WHEN "D"   MOVE 13 TO  FXD-TYOKUSO-KBN
             WHEN "E"   MOVE 14 TO  FXD-TYOKUSO-KBN
             WHEN "F"   MOVE 15 TO  FXD-TYOKUSO-KBN
             WHEN "G"   MOVE 16 TO  FXD-TYOKUSO-KBN
             WHEN "H"   MOVE 17 TO  FXD-TYOKUSO-KBN
             WHEN "I"   MOVE 18 TO  FXD-TYOKUSO-KBN
             WHEN "J"   MOVE 19 TO  FXD-TYOKUSO-KBN
             WHEN "K"   MOVE 20 TO  FXD-TYOKUSO-KBN
             WHEN "L"   MOVE 21 TO  FXD-TYOKUSO-KBN
             WHEN "M"   MOVE 22 TO  FXD-TYOKUSO-KBN
             WHEN "N"   MOVE 23 TO  FXD-TYOKUSO-KBN
             WHEN "O"   MOVE 24 TO  FXD-TYOKUSO-KBN
             WHEN "P"   MOVE 25 TO  FXD-TYOKUSO-KBN
             WHEN "Q"   MOVE 26 TO  FXD-TYOKUSO-KBN
             WHEN "R"   MOVE 27 TO  FXD-TYOKUSO-KBN
             WHEN "S"   MOVE 28 TO  FXD-TYOKUSO-KBN
             WHEN "T"   MOVE 29 TO  FXD-TYOKUSO-KBN
             WHEN "U"   MOVE 30 TO  FXD-TYOKUSO-KBN
             WHEN "V"   MOVE 31 TO  FXD-TYOKUSO-KBN
             WHEN "W"   MOVE 32 TO  FXD-TYOKUSO-KBN
             WHEN "X"   MOVE 33 TO  FXD-TYOKUSO-KBN
             WHEN "Y"   MOVE 34 TO  FXD-TYOKUSO-KBN
             WHEN "Z"   MOVE 35 TO  FXD-TYOKUSO-KBN
             WHEN OTHER MOVE 1  TO  FXD-TYOKUSO-KBN
         END-EVALUATE
     END-IF.
*甘木の場合は通常に変更する。
     IF  ST3-F05  = "E1"
         MOVE ZERO          TO  FXD-TYOKSO-KBN
         MOVE ZERO          TO  FXD-TYOKUSO-KBN
     END-IF.
*商品CD（８桁）インストア
*****PERFORM  VARYING IX  FROM 13 BY -1
*             UNTIL   IX < 1
*      IF ST3-F10 (IX:1)  IS NUMERIC
*         MOVE  ST3-F10 (1:IX)  TO  FXD-SYOCD *> 相手商品ＣＤ
*         MOVE  1           TO  IX
*      END-IF
*****END-PERFORM.
     MOVE     KH1-F13       TO  FXD-SYOCD.
*商品名＋規格名
     IF  FG-NFMEIMS-INV = 1
         MOVE  SPACE        TO  FXD-HINMN
         MOVE  SPACE        TO  FXD-KIKAK-NM
     ELSE
         MOVE  SYO-F05      TO  FXD-HINMN
         MOVE  SYO-F06      TO  FXD-KIKAK-NM
     END-IF.
*名称編集の為制御バイト転送
     MOVE  X"28"            TO  FXD-SI01.
     MOVE  X"29"            TO  FXD-SO01.
     MOVE  X"28"            TO  FXD-SI02.
     MOVE  X"29"            TO  FXD-SO02.
*発注数
*****MOVE  KH1-F19          TO  FXD-HACH-SU.
     COMPUTE FXD-HACH-SU = KH1-F19 * 10.
*納品数
*****MOVE  ST3-F11          TO  FXD-NOHN-SU.
     COMPUTE FXD-NOHN-SU = ST3-F11 * 10.
*訂正区分
     MOVE  ST3-F12          TO  FXD-TEISEI-RIY-KBN.
     IF    ST3-F12 = SPACE
           MOVE "00"        TO  FXD-TEISEI-RIY-KBN
     ELSE
           MOVE  ST3-F12    TO  FXD-TEISEI-RIY-KBN
     END-IF.
*原単価
*****MOVE  KH1-ME10         TO  FXD-GTANKA-ZNUKI.
     COMPUTE FXD-GTANKA-ZNUKI = KH1-ME10 * 100.
*売単価
     MOVE  KH1-ME12         TO  FXD-UTANKA-ZNUKI.
*店舗CD
     MOVE  ST3-F06          TO  FXD-NOHNSK-TENCD.
*納品場所CD
     MOVE  ST3-F07          TO  FXD-NOHN-BASYCD.
*JANCD
*****IF  FG-NFMEIMS-INV = 1
*        MOVE  ZERO         TO  FXD-JANCD
*    ELSE
*        MOVE  SYO-F04      TO  FXD-JANCD
*****END-IF.
     MOVE  ST3-F10          TO  FXD-JANCD.
*カンマセット
     MOVE  ","              TO  FXD-FIL-001  FXD-FIL-002.
     MOVE  ","              TO  FXD-FIL-003  FXD-FIL-004.
     MOVE  ","              TO  FXD-FIL-005  FXD-FIL-006.
     MOVE  ","              TO  FXD-FIL-007  FXD-FIL-008.
     MOVE  ","              TO  FXD-FIL-009  FXD-FIL-010.
     MOVE  ","              TO  FXD-FIL-011  FXD-FIL-012.
     MOVE  ","              TO  FXD-FIL-013  FXD-FIL-014.
     MOVE  ","              TO  FXD-FIL-015  FXD-FIL-016.
     MOVE  ","              TO  FXD-FIL-017  FXD-FIL-018.
     MOVE  ","              TO  FXD-FIL-019  FXD-FIL-020.
     MOVE  ","              TO  FXD-FIL-021.
*レコード出力
     WRITE  FXD-REC.
     ADD  1  TO  OT-CNT3.
*
 EDWT-NFSUTEDT-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品名称マスタ検索                                  *
****************************************************************
 RD-NFMEIMS-SEC          SECTION.
*
     READ  NFMEIMS
       INVALID
         MOVE  1                 TO  FG-NFMEIMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFMEIMS-INV
     END-READ.
*
 RD-NFMEIMS-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル検索                                  *
****************************************************************
 RD-NFJOHOF-SEC          SECTION.
*
     READ  NFJOHOF
       INVALID
         MOVE  1                 TO  FG-NFJOHOF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFJOHOF-INV
     END-READ.
*
 RD-NFJOHOF-EXIT.
     EXIT.

****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE  "END-SEC"        TO  S-NAME.
*
     DISPLAY  "NFHAKDT OT = "  OT-CNT   UPON CONS.
     DISPLAY  "NFSUTDT OT = "  OT-CNT2  UPON CONS.
     DISPLAY  "NFFAXDT OT = "  OT-CNT3  UPON CONS.
* ファイルのＯＰＥＮ
     CLOSE  NFHAKOF.
     CLOSE  NFSUTEF.
     CLOSE  NFJOHOF.
     CLOSE  SAKUBAF.
     CLOSE  NFMEIMS.
     CLOSE  NFHAKDT.
     CLOSE  NFSUTDT.
     CLOSE  NFFAXDT.

 END-EXIT.
     EXIT.
*****************<<  SSY3766A   END PROGRAM  >>******************

```
