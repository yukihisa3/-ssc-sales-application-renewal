# SNA0090I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0090I.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：連携指定（共通）
*    作成日／更新日　：2011/11/10
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      各種機能へ連携Ｎｏ条件を与えるための指定機能。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0090I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/10.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
     SELECT  DSPFILE
       ASSIGN    TO        GS-DSPF
       FORMAT              DSP-FMT
       GROUP               DSP-GRP
       PROCESSING          DSP-PRO
       FUNCTION            DSP-FNC
       FILE      STATUS    DSP-ST.

*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01  *> 連携Ｎｏ
       FILE STATUS     KAN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
     LABEL     RECORD    IS   OMITTED.
     COPY      FNA00901  OF   XMDLIB
     JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT            PIC  X(08).
     03  DSP-GRP            PIC  X(08).
     03  DSP-PRO            PIC  X(02).
     03  DSP-FNC            PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG           PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG            PIC  9(02)  VALUE  ZERO.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-NARRUIF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-NARKANF-INV     PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
     03  S-NAME-SV          PIC  X(30).
***  プログラムスイッチ（画面遷移制御）
     03  PSW                PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI          PIC  9(01)  VALUE  ZERO.
     03  WK-SEL-KANRNO      PIC  9(08)  VALUE  ZERO.
     03  WK-RD-KANRNO       PIC  9(08)  VALUE  ZERO.
*
     03  WK-JNOHNBI-ST      PIC  9(08).
     03  WK-JNOHNBI-ED      PIC  9(08).
*
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).
*
     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).
*
     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).
*
     03  WK-BATNO.
       05  WK-BATNO-YMD     PIC  9(08).
       05  WK-BATNO-TIME    PIC  9(04).
       05  WK-BATNO-TORCD   PIC  9(08).
     03  CT-SEQ             PIC  9(03).
     03  CT-PAGE            PIC  9(04).
     03  IX                 PIC  9(04).
     03  IX-TBL             PIC  9(04).
     03  IX-GYO             PIC  9(03).
     03  CT-SEL             PIC  9(03).
     03  FG-SEL             PIC  9(01).
     03  WK-SELGYO          PIC  9(02).
     03  WK-SDENNO          PIC  9(09).
     03  WK-ONLTEG          PIC  X(01).
*
 01  WK-MIDASHI.
     03  WK-MIDASHI-ONL     PIC  N(21)  VALUE
     NC"バッチＮｏ　　　　　　　　　場所　納品日　".
     03  WK-MIDASHI-TEG     PIC  N(21)  VALUE
     NC"場所　抽出取引先　抽出伝票Ｎｏ範囲　　　　".
*
 01  WK-KEY-ONL.
     03  FILLER             PIC  X(01)  VALUE SPACE.
     03  WK-KEY-ONL-BATHZK  PIC  9(08).
     03  FILLER             PIC  X(03)  VALUE " - ".
     03  WK-KEY-ONL-BATHM   PIC  9(04).
     03  FILLER             PIC  X(03)  VALUE " - ".
     03  WK-KEY-ONL-BATTOR  PIC  9(08).
     03  FILLER             PIC  X(02)  VALUE SPACE.
     03  WK-KEY-ONL-BASHO   PIC  X(02).
     03  FILLER             PIC  X(04)  VALUE SPACE.
     03  WK-KEY-ONL-NOHNBI  PIC  9(08).
*
 01  WK-KEY-TEG.
     03  FILLER             PIC  X(01)  VALUE SPACE.
     03  WK-KEY-TEG-BASHO   PIC  X(02).
     03  FILLER             PIC  X(04)  VALUE SPACE.
     03  WK-KEY-TEG-SELTOR  PIC  9(08).
     03  FILLER             PIC  X(04)  VALUE SPACE.
     03  WK-KEY-TEG-SELSDEN PIC  9(09).
     03  FILLER             PIC  X(03)  VALUE " - ".
     03  WK-KEY-TEG-SELEDEN PIC  9(09).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH              PIC   9(02)  VALUE  ZERO.
     03  WK-MM              PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了　_項目戻り".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS 2.
*
*メッセージの取得
*A---B-------2---------3---------4---------5---------6---------7E*
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"連携Ｎｏ（開始）が未入力です。入力して下さい。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
             NC"指定された連携Ｎｏは存在しません。再指定して下さ
-              "い。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
         NC"連携Ｎｏ（開始・終了）の大小が逆です。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG11.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG13.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG14.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG15.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG16.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG17.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG18.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG19.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG20.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R          PIC  N(30)  OCCURS 20.
*
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(20)  VALUE
         NC"画面ファイルエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  PAR-IN-BMNCD           PIC  X(04).
 01  PAR-IN-TANCD           PIC  X(02).
*
 01  PAR-IN-MSG1            PIC  N(36).
 01  PAR-IN-MSG2            PIC  N(36).
 01  PAR-IN-MSG3            PIC  N(36).
 01  PAR-IN-MSG4            PIC  N(36).
 01  PAR-IN-MSG5            PIC  N(36).
*
* 出力パラメータ
*  パラメータＢ
 01  PAB-TANCD              PIC  X(06).
 01  PAB-ONLTEG             PIC  X(01).
 01  PAB-RENNO-FROM         PIC  X(09).
 01  PAB-RENNO-TO           PIC  X(09).
 01  PAB-IPADDR             PIC  X(15).
*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PAR-IN-BMNCD
                                  PAR-IN-TANCD

                                  PAR-IN-MSG1
                                  PAR-IN-MSG2
                                  PAR-IN-MSG3
                                  PAR-IN-MSG4
                                  PAR-IN-MSG5

                                  PAB-TANCD
                                  PAB-ONLTEG
                                  PAB-RENNO-FROM
                                  PAB-RENNO-TO
                                  PAB-IPADDR.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.
*
     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
* ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT NARKANF.
* ワークの初期化
     INITIALIZE  FLG-AREA.
     MOVE  ZERO             TO  PSW.
* 初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
* 初期画面へ
     MOVE  "1"              TO  PSW.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.
*
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*
*I↓************************************************************
*  項目クリア
*    開始連携Ｎｏ、確認入力
*    IF  PSW = "1" OR "3" OR ZERO
*       MOVE  SPACE         TO  DSP-FNA00901
*    END-IF.
*
*    終了連携Ｎｏ
*    IF  PSW = "2"
*        MOVE  SPACE        TO  DSP-ERENNO(1:9)
*        MOVE  SPACE        TO  DSP-EMIDAS
*        MOVE  SPACE        TO  DSP-EKEY
*    END-IF.
*
*  リバース，カーソルパーク解除
*    EVALUATE  PSW
*    担当者入力
*      WHEN  "1"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-SRENNO
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SRENNO
*
*    状態指定入力
*      WHEN  "2"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-ERENNO
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-ERENNO
*
*    確認入力
*      WHEN  "3"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-KAKUNI
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KAKUNI
*
*    END-EVALUATE.
****************************************************************
     MOVE  SPACE            TO  DSP-FNA00901.
     MOVE  SPACE            TO  DSP-ERENNO(1:9).
     MOVE  SPACE            TO  DSP-EMIDAS.
     MOVE  SPACE            TO  DSP-EKEY.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SRENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SRENNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-ERENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-ERENNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KAKUNI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KAKUNI.
*I↑************************************************************
*
*  ＰＧＩＤ
     MOVE  "SNA0090I"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
* メッセージ
     MOVE  PAR-IN-MSG1      TO  DSP-COMENT (1).
     MOVE  PAR-IN-MSG2      TO  DSP-COMENT (2).
     MOVE  PAR-IN-MSG3      TO  DSP-COMENT (3).
     MOVE  PAR-IN-MSG4      TO  DSP-COMENT (4).
     MOVE  PAR-IN-MSG5      TO  DSP-COMENT (5).
*
     MOVE  S-NAME-SV        TO  S-NAME.
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
     EVALUATE  PSW
*    開始連携Ｎｏ
       WHEN  "1"  PERFORM  DSP-SRENNO-SEC
*    終了連携Ｎｏ
       WHEN  "2"  PERFORM  DSP-ERENNO-SEC
*    確認
       WHEN  "3"  PERFORM  DSP-KAKU-SEC
*    その他
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  開始連携Ｎｏ項目  ( PSW = 1 )                               *
****************************************************************
 DSP-SRENNO-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-SRENNO-SEC" TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  SRENNO-CHK-SEC
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
*    その他
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*  画面表示処理                                                *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.
*
     PERFORM  SDATE-GET-SEC.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*  エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE        TO  DSP-ERRMSG
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG) TO  DSP-ERRMSG
         MOVE  ZERO         TO  ERR-FLG
     END-IF.
*  ガイドメッセージの設定（次入力モードに切換後）
     EVALUATE  PSW
*    メイン入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
       WHEN  "2"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
       WHEN  "3"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID
     END-EVALUATE.
*
*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNA00901"       TO  DSP-FMT.
     WRITE  DSP-FNA00901.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.
*
     MOVE  "NE"             TO  DSP-PRO.
*
     EVALUATE  PSW
*    開始連携Ｎｏ
       WHEN  "1"
         MOVE  "SRENNO"     TO  DSP-GRP
*    終了連携Ｎｏ
       WHEN  "2"
         MOVE  "ERENNO"     TO  DSP-GRP
*    確認
       WHEN  "3"
         MOVE  "KAKU"       TO  DSP-GRP
     END-EVALUATE.
*
     MOVE  "FNA00901"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
     MOVE  SPACE            TO   DSP-PRO.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  開始連携Ｎｏチェック                                        *
****************************************************************
 SRENNO-CHK-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SRENNO-CHK-SEC" TO  S-NAME.
*
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SRENNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SRENNO.
*
     MOVE  SPACE            TO  DSP-SMIDAS.
     MOVE  SPACE            TO  DSP-SKEY.
*
     MOVE  SPACE            TO  WK-ONLTEG.
*
     IF  DSP-SRENNO = SPACE OR ZERO
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SRENNO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SRENNO
         IF  DSP-SRENNO = ZERO
             MOVE  SPACE    TO  DSP-SRENNO (1:9)
         END-IF
         GO TO  SRENNO-CHK-090
     END-IF.
*
* 連携Ｎｏ管理テーブル
     MOVE  DSP-SRENNO       TO  KAN-F01.
     PERFORM  RD-NARKANF-SEC.
     IF  FG-NARKANF-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SRENNO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SRENNO
         GO TO  SRENNO-CHK-090
     END-IF.
*
     MOVE  KAN-F071         TO  WK-ONLTEG.

     IF  KAN-F071 = "1"  *> 手書き、入力区分
         MOVE  WK-MIDASHI-TEG  TO  DSP-SMIDAS
         MOVE  KAN-F073     TO  WK-KEY-TEG-BASHO
         MOVE  KAN-F076     TO  WK-KEY-TEG-SELTOR
         MOVE  KAN-F077     TO  WK-KEY-TEG-SELSDEN
         MOVE  KAN-F078     TO  WK-KEY-TEG-SELEDEN
         MOVE  WK-KEY-TEG   TO  DSP-SKEY
     ELSE                *> オンライン、入力区分
         MOVE  WK-MIDASHI-ONL  TO  DSP-SMIDAS
         MOVE  KAN-F0721    TO  WK-KEY-ONL-BATHZK
         MOVE  KAN-F0722    TO  WK-KEY-ONL-BATHM
         MOVE  KAN-F0723    TO  WK-KEY-ONL-BATTOR
         MOVE  KAN-F073     TO  WK-KEY-ONL-BASHO
         MOVE  KAN-F074     TO  WK-KEY-ONL-NOHNBI
         MOVE  WK-KEY-ONL   TO  DSP-SKEY
     END-IF.
*
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  SRENNO-CHK-090
     END-IF.
*
     MOVE  2                TO  PSW.
 SRENNO-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 SRENNO-CHK-EXIT.
     EXIT.
****************************************************************
*    連携Ｎｏ管理テーブル検索                                  *
****************************************************************
 RD-NARKANF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARKANF-SEC"  TO  S-NAME.
*
     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARKANF-EXIT.
     EXIT.
****************************************************************
*  終了連携Ｎｏ項目  ( PSW = 2 )                               *
****************************************************************
 DSP-ERENNO-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-ERENNO-SEC" TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  ERENNO-CHK-SEC
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    項目戻し
       WHEN  "F006"
         MOVE  "1"          TO  PSW
*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-ERENNO-EXIT.
     EXIT.
****************************************************************
*  終了連携Ｎｏチェック                                        *
****************************************************************
 ERENNO-CHK-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "ERENNO-CHK-SEC" TO  S-NAME.
*
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-ERENNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-ERENNO.
*
     MOVE  SPACE            TO  DSP-EMIDAS.
     MOVE  SPACE            TO  DSP-EKEY.
*
* 連携Ｎｏ管理テーブル
     IF  DSP-ERENNO = SPACE OR ZERO
         MOVE  DSP-SRENNO   TO  DSP-ERENNO
     END-IF.
*
     MOVE  DSP-ERENNO       TO  KAN-F01.
     PERFORM  RD-NARKANF-SEC.
     IF  FG-NARKANF-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-ERENNO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-ERENNO
         GO TO  ERENNO-CHK-090
     END-IF.
*
     IF  KAN-F071 = "1"  *> 手書き、入力区分
         MOVE  WK-MIDASHI-TEG  TO  DSP-EMIDAS
         MOVE  KAN-F073     TO  WK-KEY-TEG-BASHO
         MOVE  KAN-F076     TO  WK-KEY-TEG-SELTOR
         MOVE  KAN-F077     TO  WK-KEY-TEG-SELSDEN
         MOVE  KAN-F078     TO  WK-KEY-TEG-SELEDEN
         MOVE  WK-KEY-TEG   TO  DSP-EKEY
     ELSE                *> オンライン、入力区分
         MOVE  WK-MIDASHI-ONL  TO  DSP-EMIDAS
         MOVE  KAN-F0721    TO  WK-KEY-ONL-BATHZK
         MOVE  KAN-F0722    TO  WK-KEY-ONL-BATHM
         MOVE  KAN-F0723    TO  WK-KEY-ONL-BATTOR
         MOVE  KAN-F073     TO  WK-KEY-ONL-BASHO
         MOVE  KAN-F074     TO  WK-KEY-ONL-NOHNBI
         MOVE  WK-KEY-ONL   TO  DSP-EKEY
     END-IF.

     IF  DSP-SRENNO > DSP-ERENNO
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-ERENNO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-ERENNO
         GO TO  ERENNO-CHK-090
     END-IF.
*
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  ERENNO-CHK-090
     END-IF.
*
     MOVE  5                TO  ERR-FLG. *> 確認メッセージ
     MOVE  3                TO  PSW.
 ERENNO-CHK-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 ERENNO-CHK-EXIT.
     EXIT.
****************************************************************
*  確認処理入力 （ PSW = 3 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  PARA-OT-SEC
         MOVE  "END"        TO  END-FLG
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW
*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
*    その他
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-KAKU-SEC
     END-EVALUATE.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "PARA-OT-SEC"    TO  S-NAME.
*
* オンライン/手書種別
     MOVE  WK-ONLTEG        TO  PAB-ONLTEG.
* 部門・担当者ＣＤ
     MOVE  SPACE            TO  PAB-TANCD.
     STRING
       PAR-IN-BMNCD  PAR-IN-TANCD
       DELIMITED BY SIZE  INTO  PAB-TANCD.
* 開始連携Ｎｏ
     MOVE  DSP-SRENNO       TO  PAB-RENNO-FROM.
* 終了連携Ｎｏ
     MOVE  DSP-ERENNO       TO  PAB-RENNO-TO.
* ＩＰアドレス
     MOVE  SPACE            TO  PAB-IPADDR.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  NARKANF.
     CLOSE  DSPFILE.
*
     MOVE  S-NAME-SV        TO  S-NAME.
*
 END-EXIT.
     EXIT.
*****************<<  SNA0090I   END PROGRAM  >>******************

```
