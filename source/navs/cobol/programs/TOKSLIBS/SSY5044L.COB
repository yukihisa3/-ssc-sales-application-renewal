****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　新ＥＤＩオンラインシステム　　　　*
*    モジュール名　　　　：　納品明細発行                      *
*    作成日／更新日　　　：　2010/04/06                        *
*    作成者／更新者　　　：　NAV OONO                          *
*    処理概要　　　　　　：　納品明細発行制御Ｆを順に読み、店舗*
*                            マスタを参照しながら、納品明細書を*
*                            発行する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　2020/11/09                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　会社名変更（ビバホーム）　　　　　*
*    更新日／更新者　　　：　2020/12/21 INOUE                  *
*    更新概要　　　　　　：　ヘッダに取引先コード印字追加　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5044L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/04/06.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE         IS      CONS
*
     YA              IS      PITCH-2
     YB              IS      PITCH-15
     YB-21           IS      PITCH-3
     YB-22           IS      YB-22.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE      STATUS    JYO-STATUS.
*（リスト）納品明細発行制御ファイル（ＬＦ１）
     SELECT   LSTNMEF   ASSIGN    TO        DA-01-VI-LSTNMEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       LST-F01   LST-F02
                                            LST-F03   LST-F04
                                            LST-F05   LST-F07
                        FILE      STATUS    LST-STATUS.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*店舗マスタ
******************************************************************
 FD  HTENMS
                        LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
******************************************************************
*条件ファイル
******************************************************************
 FD  JYOKEN1
                        LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO  AS   PREFIX.
*
******************************************************************
*（リスト）納品明細発行制御ファイル（ＬＦ１）
******************************************************************
 FD  LSTNMEF            LABEL RECORD   IS   STANDARD.
     COPY     LSTNMEF   OF        XFDLIB
              JOINING   LST       PREFIX.
*
*****************************************************************
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED
     LINAGE      IS        80        LINES
     DATA        RECORD    IS        PRT-REC.
 01  PRT-REC               PIC X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*****************************************************************
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  PRT-FORM          PIC X(08).
     03  PRT-PROC          PIC X(02).
     03  PRT-GRP           PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL     PIC X(04).
         05  PRT-STR-PG    PIC X(02).
*
*ステータス領域
 01  WK-ST.
     03  TEN-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
     03  LST-STATUS          PIC  X(02).
     03  PRT-STATUS          PIC  X(02).
*フラグ領域
 01  WK-FLG.
     03  END-FLG             PIC  X(03)     VALUE  SPACE.
     03  KEP-FLG             PIC  X(01)     VALUE  SPACE.
     03  SEQ-FLG             PIC  X(01)     VALUE  SPACE.
*カウント領域
 01  WK-CNT.
     03  M1-CNT              PIC  9(02)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
*
*データ退避
 01  WK-SYSBI.
     03   WK-SYS-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-SYS-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-SYS-DD      PIC  9(02).
*
 01  WK-NOUDT.
     03   WK-NOU-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-NOU-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-NOU-DD      PIC  9(02).
*
 01  WK-HATDT.
     03   WK-HAT-YY      PIC  9(04).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-HAT-MM      PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   "/".
     03   WK-HAT-DD      PIC  9(02).
*
 01  WK-RIYUCD.
     03   WK-RIYU-CD     PIC  9(02).
     03   FILLER         PIC  X(01)    VALUE   ":".
 01  WK-RIYUCD-R         REDEFINES  WK-RIYUCD.
     03   WK-RIYUCD-R2   PIC  X(03).
*メッセージ領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5044L".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY5044L".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SSY5044L".
         05  FILLER          PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
     03  MSG-IN.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC   9(06).
         05  FILLER          PIC   X(05)  VALUE " *** ".
*ＦＬレコード
 01  WK-FL-REC.
     03  WK-FL-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F02           PIC   9(05)   VALUE  ZERO.
     03  WK-FL-F03           PIC   9(08)   VALUE  ZERO.
     03  WK-FL-F04           PIC   X(04)   VALUE  SPACE.
     03  WK-FL-F05           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード
 01  WK-M1-REC.
     03  WK-M1-F01           PIC   9(08)   VALUE  ZERO.
     03  WK-M1-F02           PIC   9(06)   VALUE  ZERO.
     03  WK-M1-F03           PIC   N(06)   VALUE  SPACE.
     03  WK-M1-F04           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F05           PIC   X(01)   VALUE  SPACE.
     03  WK-M1-F06           PIC   X(01)   VALUE  SPACE.
*Ｍ１レコード
 01  WK-M2-REC.
     03  WK-M2-F01           PIC  9(02)    VALUE  ZERO.
     03  WK-M2-F02           PIC  X(25)    VALUE  SPACE.
     03  WK-M2-F03           PIC  X(15)    VALUE  SPACE.
     03  WK-M2-F04           PIC  X(08)    VALUE  SPACE.
     03  WK-M2-F05           PIC  X(13)    VALUE  SPACE.
     03  WK-M2-F06           PIC  9(04)    VALUE  ZERO.
     03  WK-M2-F07           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F08           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F09           PIC  9(05)    VALUE  ZERO.
     03  WK-M2-F10           PIC  X(02)    VALUE  SPACE.
     03  WK-M2-F11           PIC  9(02)    VALUE  ZERO.
*
  01  LINK-AREA.
      03  LINK-IN-KBN        PIC   X(01).
      03  LINK-IN-YMD6       PIC   9(06).
      03  LINK-IN-YMD8       PIC   9(08).
      03  LINK-OUT-RET       PIC   X(01).
      03  LINK-OUT-YMD8      PIC   9(08).
*
*
*************************
*    ﾐﾀﾞｼ  1  ｷﾞｮｳﾒ     *
*************************
*
 01  HEAD01          CHARACTER   TYPE   YB-22.
     02  FILLER      PIC X(27)   VALUE  SPACE.
     02  FILLER      PIC N(15)   VALUE
*#2020/11/09 NAV ST
*********NC"ＬＩＸＩＬビバ納品明細書".
         NC"　ビバホーム納品明細書　".
*#2020/11/09 NAV ED
*
 01  HEAD02.
     02  FILLER      PIC X(68)   VALUE  SPACE.
     02  SDATE       PIC 9(10).
     02  FILLER      PIC X(01)   VALUE  SPACE.
     02  STIME       PIC 9(05).
     02  FILLER      PIC X(03)   VALUE  SPACE.
     02  TPAGE       PIC 9(04).
     02  FILLER      PIC X(01)   VALUE  "/".
     02  SPAGE       PIC 9(04).
*
 01  HEAD03          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(04)  VALUE  NC"店舗名：".
     02  TENCD       PIC XXXX.
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  TENNM       PIC N(10).
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC N(06)  VALUE  NC"納品予定日：".
     02  NOUDT       PIC X(10).
*
 01  HEAD04          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(05)  VALUE  NC"取引先名：".
*↓2021.12.21
     02  TOKCD       PIC 9(05).
     02  FILLER      PIC X(01)  VALUE  SPACE.
*↑2021.12.21
     02  TOKNM       PIC N(10).
     02  FILLER      PIC X(08)  VALUE  SPACE.
     02  FILLER      PIC N(02)  VALUE  NC"分類".
     02  FILLER      PIC X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER      PIC N(01)  VALUE  NC"：".
     02  BUNCD       PIC X(04).
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"欠品：".
     02  KEPIN       PIC N(01).
*
 01  SEN1            CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC N(46)  VALUE  ALL  NC"─".
*
 01  HEAD05          CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"発注日".
     02  FILLER      PIC X(20)  VALUE  SPACE.
     02  FILLER      PIC N(04)  VALUE  NC"伝票番号".
     02  FILLER      PIC X(19)  VALUE  SPACE.
     02  FILLER      PIC N(02)  VALUE  NC"客注".
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(02)  VALUE  NC"緊急".
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(02)   VALUE  NC"特売".
*
 01  HEAD10.
     02  FILLER      PIC X(03)   VALUE  "ﾘｽﾄ".
*
 01  HEAD11.
     02  FILLER      PIC N(02)  VALUE  NC"通番"
                     CHARACTER  TYPE   PITCH-15.
*
 01  SEN2            CHARACTER   TYPE   PITCH-2.
     02  FILLER      PIC X(03)   VALUE  SPACE.
     02  FILLER      PIC N(01)   VALUE  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(13)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(07)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┬".
     02  FILLER      PIC N(06)   VALUE  ALL  NC"─".
*
 01  HEAD06.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"行"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC N(03)  VALUE  NC"商品名"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(17)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(02)  VALUE  SPACE.
     02  FILLER      PIC N(02)  VALUE  NC"商品"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  "ｺｰﾄﾞ".
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"発注単位"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"検収数量"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
*
 01  SEN3.
     02  FILLER      PIC X(34)   VALUE  SPACE.
     02  FILLER      PIC X(24)   VALUE  ALL  "_".
*
 01  HEAD07.
     02  FILLER      PIC X(05)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(26)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC X(07)  VALUE  "JANｺｰﾄﾞ".
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"発注数量"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"出荷数量"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"欠品数量"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"チェック"
                     CHARACTER  TYPE   PITCH-15.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC N(04)  VALUE  NC"欠品理由"
                     CHARACTER  TYPE   PITCH-15.
*
 01  SEN4            CHARACTER   TYPE   PITCH-2.
     02  FILLER      PIC X(03)   VALUE  SPACE.
     02  FILLER      PIC N(01)   VALUE  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(13)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(07)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┴".
     02  FILLER      PIC N(06)   VALUE  ALL  NC"─".
*
 01  SEN5            CHARACTER   TYPE   PITCH-2.
     02  FILLER      PIC X(03)   VALUE  SPACE.
     02  FILLER      PIC N(01)   VALUE  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(13)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(07)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(03)   VALUE  ALL  NC"─".
     02  FILLER      PIC N(01)   VALUE  NC"┼".
     02  FILLER      PIC N(06)   VALUE  ALL  NC"─".
*
*************************
*    ﾒｲｻｲ     ｷﾞｮｳ      *
*************************
 01  MEISAI01.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  HATDT       PIC X(10).
     02  FILLER      PIC X(16)  VALUE  SPACE.
     02  DENNO       PIC N(06)
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(15)  VALUE  SPACE.
     02  KBN1        PIC N(02)
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  KBN2        PIC N(02)
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  KBN3        PIC N(02)
                     CHARACTER  TYPE   PITCH-2.
*
 01  MEISAI02.
     02  SEQNO       PIC ZZ.
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  GYO         PIC 99.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  SYONM1      PIC X(25).
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  SYOCD       PIC X(08).
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(03)  VALUE  SPACE.
     02  HATAN       PIC ZZ9.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  KEPCD       PIC X(03).
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  KEPNM1      PIC N(05)
                     CHARACTER  TYPE   PITCH-15.
*
 01  MEISAI03.
     02  FILLER      PIC X(05)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  SYONM2      PIC X(15).
     02  FILLER      PIC X(11)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  JANCD       PIC X(13).
     02  FILLER      PIC X(01)  VALUE  SPACE.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  SURYO       PIC ZZ,ZZ9.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  SYUSU       PIC ZZ,ZZ9.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  KEPSU       PIC ZZ,ZZ9.
     02  FILLER      PIC N(01)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(06)  VALUE  " (  ) ".
     02  FILLER      PIC N(02)  VALUE  NC"│"
                     CHARACTER  TYPE   PITCH-2.
     02  FILLER      PIC X(04)  VALUE  SPACE.
     02  KEPNM2      PIC N(05)
                     CHARACTER  TYPE   PITCH-15.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "HTENMS"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1"    TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LSTNMEF.
     MOVE      "LSTNMEL1"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF "      TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     HTENMS   JYOKEN1   LSTNMEF.
     OPEN     OUTPUT    PRTF.
*
     MOVE     "FSY50403" TO        PRT-FORM.
*開始メッセージ出力
     DISPLAY  MSG-START UPON CONS.
*システム日付編集（システム日付により、日付を８桁に変換）
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE      LINK-OUT-YMD8(1:4) TO   WK-SYS-YY.
     MOVE      LINK-OUT-YMD8(5:2) TO   WK-SYS-MM.
     MOVE      LINK-OUT-YMD8(7:2) TO   WK-SYS-DD.
*
     MOVE      WK-SYSBI           TO   SDATE.
*
*画面表示時刻編集
     ACCEPT    SYS-TIME           FROM TIME.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
*
     MOVE      HEN-TIME           TO   STIME.
*
*    納品明細発行制御ファイル読込
     PERFORM LSTNMEF-READ-SEC.
*    レコード区分判定
     IF          LST-F08    NOT =    "TP"
         MOVE    "END"          TO   END-FLG
*        メッセージ出力
     ELSE
*        総頁セット
         MOVE    LST-F06        TO   SPAGE
     END-IF.
*
*    納品明細発行制御ファイル読込
     PERFORM LSTNMEF-READ-SEC.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     EVALUATE      LST-F08
       WHEN
*       ＦＬ出力処理へ（伝票ヘッダ）
        "FL"
           PERFORM      FL-SEC
       WHEN
*       Ｍ１出力処理へ（明細ヘッダ）
        "M1"
           PERFORM      M1-SEC
       WHEN
*       Ｍ２出力処理へ（明細）
        "M2"
           PERFORM      M2-SEC
       WHEN
         OTHER
           GO                     TO   MAIN-EXIT
       END-EVALUATE.
*
*
     PERFORM LSTNMEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*ＦＬレコード出力処理
****************************************************************
 FL-SEC                SECTION.
*
     MOVE     "FL-SEC"             TO  S-NAME.
*各項目の初期化
     MOVE      SPACE               TO  TENNM  NOUDT  TENCD
                                       TOKNM  BUNCD  KEPIN.
     MOVE      ZERO                TO  TPAGE.
*
     INITIALIZE                        TPAGE  TENNM  NOUDT  TENCD
                                       TOKNM  BUNCD  KEPIN.
*
*ワーク領域にセット
     MOVE      LST-F10             TO  WK-FL-REC.
*頁
     MOVE      LST-F05            TO   TPAGE.
*
*店舗マスタ読込
*    取引先ＣＤセット
     MOVE     WK-FL-F01           TO   TEN-F52.
*↓2021.12.21
     MOVE     WK-FL-F01           TO   TOKCD.
*↑2021.12.21
*    店舗ＣＤセット
     MOVE     WK-FL-F02           TO   TEN-F011.
     MOVE     WK-FL-F02(2:4)      TO   TENCD.
     READ     HTENMS     INVALID
              MOVE    NC"＊＊＊＊＊＊＊＊＊＊"
                                  TO   TENNM
              NOT  INVALID
              MOVE    TEN-F03     TO   TENNM
     END-READ.
*納品日
     MOVE      WK-FL-F03(1:4)     TO   WK-NOU-YY.
     MOVE      WK-FL-F03(5:2)     TO   WK-NOU-MM.
     MOVE      WK-FL-F03(7:2)     TO   WK-NOU-DD.
*
     MOVE      WK-NOUDT           TO   NOUDT.
*
*条件ファイル読込
*    項目１セット
     MOVE      99                 TO  JYO-F01.
*    項目２セット
     MOVE     "99999999"          TO  JYO-F02.
     READ     JYOKEN1    INVALID
              MOVE    NC"＊＊＊＊＊＊＊＊＊＊"
                                  TO   TOKNM
              NOT  INVALID
              MOVE    JYO-F03     TO   TOKNM
     END-READ.
*分類ＣＤ
     MOVE      WK-FL-F04           TO  BUNCD.
*欠品区分
     IF        WK-FL-F05     =    "1"
               MOVE      NC"有"    TO  KEPIN
     ELSE
               MOVE      NC"無"    TO  KEPIN
     END-IF.
*
*出力する。
*    1頁目か判定
     IF       LST-F05 > 1
          MOVE      SPACE     TO        PRT-REC
          WRITE     PRT-REC   AFTER     PAGE
     END-IF.
     WRITE     PRT-REC   FROM HEAD01   AFTER     1.
     WRITE     PRT-REC   FROM HEAD02   AFTER     1.
     WRITE     PRT-REC   FROM HEAD03   AFTER     1.
     WRITE     PRT-REC   FROM HEAD04   AFTER     2.
     WRITE     PRT-REC   FROM SEN1     AFTER     1.
     WRITE     PRT-REC   FROM HEAD05   AFTER     1.
     WRITE     PRT-REC   FROM SEN2     AFTER     1.
     WRITE     PRT-REC   FROM HEAD10   AFTER     0.
     WRITE     PRT-REC   FROM HEAD06   AFTER     1.
     WRITE     PRT-REC   FROM SEN3     AFTER     0.
     WRITE     PRT-REC   FROM HEAD11   AFTER     0.
     WRITE     PRT-REC   FROM HEAD07   AFTER     1.
     WRITE     PRT-REC   FROM SEN4     AFTER     1.
     WRITE     PRT-REC   FROM SEN1     AFTER     1.
*リスト通番を初期化する。
     MOVE      ZERO      TO        M1-CNT.
*
 FL-EXIT.
     EXIT.
****************************************************************
*Ｍ１レコード出力処理
****************************************************************
 M1-SEC                SECTION.
*
     MOVE     "M1-SEC"             TO  S-NAME.
*各項目の初期化
     MOVE      SPACE               TO  HATDT  DENNO  KBN1
                                              KBN2   KBN3.
     INITIALIZE                        HATDT  DENNO  KBN1
                                              KBN2   KBN3.
*
*ワーク領域にセット
     MOVE      LST-F10             TO  WK-M1-REC.
*発注日
     MOVE      WK-M1-F01(1:4)      TO  WK-HAT-YY.
     MOVE      WK-M1-F01(5:2)      TO  WK-HAT-MM.
     MOVE      WK-M1-F01(7:2)      TO  WK-HAT-DD.
*
     MOVE      WK-HATDT            TO  HATDT.
*伝票番号
     MOVE      WK-M1-F03           TO  DENNO.
*客注
     IF        WK-M1-F04 =  "1"
               MOVE    NC"客注"    TO KBN1
     END-IF.
*緊急
     IF        WK-M1-F05 =  "1"
               MOVE    NC"緊急"    TO KBN2
     END-IF.
*特売
     IF        WK-M1-F06 =  "1"
               MOVE    NC"特売"    TO KBN3
     END-IF.
*
*出力する。
*
     WRITE     PRT-REC   FROM MEISAI01 AFTER     1.
     WRITE     PRT-REC   FROM SEN2     AFTER     1.
*
     ADD       1                   TO  M1-CNT.
*
*    ﾘｽﾄ通番出力フラグを立てる
     MOVE     "1"                  TO  SEQ-FLG.
*
 M1-EXIT.
     EXIT.
****************************************************************
*Ｍ２レコード出力処理
****************************************************************
 M2-SEC                SECTION.
*
     MOVE     "M2-SEC"             TO  S-NAME.
*各項目の初期化
     MOVE      SPACE               TO  SYONM1 SYONM2 SYOCD
                                       JANCD  KEPCD  KEPNM1
                                       KEPNM2.
     MOVE      ZERO                TO  SEQNO  GYO    HATAN
                                       SURYO  SYUSU  KEPSU.
*
     INITIALIZE                        SEQNO  GYO    SYONM1
                                       SYONM2 SYOCD  JANCD
                                       HATAN  SURYO  SYUSU
                                       KEPSU  KEPCD  KEPNM1
                                       KEPNM2.
*
*ワーク領域にセット
     MOVE      LST-F10             TO  WK-M2-REC.
*ﾘｽﾄ通番
     IF  SEQ-FLG   =    "1"
         MOVE      M1-CNT              TO  SEQNO
*        ﾘｽﾄ通番出力フラグを折る
         MOVE      SPACE               TO  SEQ-FLG
     END-IF.
*行数
     MOVE      WK-M2-F01           TO  GYO.
*商品名１、２
     MOVE      WK-M2-F02           TO  SYONM1.
     MOVE      WK-M2-F03           TO  SYONM2.
*商品ＣＤ
     MOVE      WK-M2-F04           TO  SYOCD.
*ＪＡＮＣＤ
     MOVE      WK-M2-F05           TO  JANCD.
*発注単位
     MOVE      WK-M2-F06           TO  HATAN.
*発注数
     MOVE      WK-M2-F07           TO  SURYO.
*出荷数
     MOVE      WK-M2-F08           TO  SYUSU.
*欠品数
     MOVE      WK-M2-F09           TO  KEPSU.
*欠品区分
     MOVE      WK-M2-F10           TO  WK-RIYU-CD.
*欠品区分が空白の場合は、出力しない。
     IF  WK-M2-F10  NOT =  SPACE
         MOVE      WK-RIYUCD       TO  KEPCD
     ELSE
         MOVE      SPACE           TO  KEPCD
     END-IF.
*
     EVALUATE     WK-M2-F10
         WHEN     11
                  MOVE       NC"マスタメン"   TO   KEPNM1
                  MOVE       NC"テミス"       TO   KEPNM2
         WHEN     12
                  MOVE       NC"発注ミス"     TO   KEPNM1
                  MOVE       SPACE            TO   KEPNM2
         WHEN     21
                  MOVE       NC"納入者品切"   TO   KEPNM1
                  MOVE       SPACE            TO   KEPNM2
         WHEN     22
                  MOVE       NC"メーカ品切"   TO   KEPNM1
                  MOVE       SPACE            TO   KEPNM2
         WHEN     OTHER
                  CONTINUE
     END-EVALUATE.
*出力する。
*
     WRITE     PRT-REC   FROM MEISAI02 AFTER     1.
     WRITE     PRT-REC   FROM SEN3     AFTER     0.
     WRITE     PRT-REC   FROM MEISAI03 AFTER     1.
     IF   WK-M2-F01  =  WK-M2-F11
          WRITE     PRT-REC   FROM SEN4     AFTER     1
     ELSE
          WRITE     PRT-REC   FROM SEN5     AFTER     1
     END-IF.
*
 M2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "出力頁数  = " LST-F05   UPON CONS.
*
     CLOSE     HTENMS   JYOKEN1  LSTNMEF  PRTF.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　納品明細発行制御ファイル読込
****************************************************************
 LSTNMEF-READ-SEC    SECTION.
*
     READ     LSTNMEF
              AT  END
                  MOVE     "END"    TO  END-FLG
     DISPLAY "納品明細発行制御Ｆを読み終えました。"
                                      UPON CONS
                  GO                TO  LSTNMEF-READ-EXIT
     END-READ.
*
 LSTNMEF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
