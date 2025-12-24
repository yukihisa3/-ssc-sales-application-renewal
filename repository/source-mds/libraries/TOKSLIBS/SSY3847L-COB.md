# SSY3847L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3847L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援システム            *
*    モジュール名　　　　：　本発ＥＸＣＥＬ確定ＤＴ取込結果    *
*    作成日／更新日　　　：　2015/05/25                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより範囲を受取、取込箱数　*
*                            確定ファイルを読み、エラー分のリ　*
*                            ストを発行する。                  *
*    作成日／更新日　　　：　2020/07/07                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　日付範囲チェック追加　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3847L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/05/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取込数量確定ファイル
     SELECT   TRKAKUF   ASSIGN    TO        DA-01-VI-TRKAKUL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAK-F01   KAK-F02
                                            KAK-F03   KAK-F110
                                            KAK-F111  KAK-F112
                                            KAK-F113  KAK-F11H
                                            KAK-F115  KAK-F116
                                            KAK-F119  KAK-F11A
                                            KAK-F11B
                        FILE  STATUS   IS   KAK-STATUS.
*取込箱数確定ファイル
     SELECT   TRHAKOF   ASSIGN    TO        DA-01-VI-TRHAKOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 THK-F06
                                            THK-F02   THK-F03
                                            THK-F01   THK-F121
                                            THK-F122  THK-F123
                                            THK-F124  THK-F12B
                                            THK-F126  THK-F127
                                            THK-F128
                        FILE  STATUS   IS   THK-STATUS.
*店舗マスタ
     SELECT      NFTENMS     ASSIGN    TO       DA-01-VI-NFTENMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      TEN-F01   TEN-F02
                             FILE      STATUS   TEN-STATUS.
*作場マスタ
     SELECT     SAKUMS       ASSIGN    TO       DA-01-VI-SAKUBAL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SAK-F01
                             FILE      STATUS   SAK-STATUS.
*担当者マスタ
     SELECT     HTANMS       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*ナフコ商品マスタ
     SELECT     NFSHOMS       ASSIGN    TO      DA-01-VI-NFSHOMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F01
                             FILE      STATUS   MEI-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    取込数量確定ファイル
 FD  TRKAKUF            LABEL RECORD   IS   STANDARD.
     COPY     TRKAKUF   OF        XFDLIB
              JOINING   KAK       PREFIX.
*    取込箱数確定ファイル
 FD  TRHAKOF            LABEL RECORD   IS   STANDARD.
     COPY     TRHAKOF   OF        XFDLIB
              JOINING   THK       PREFIX.
*店舗マスタ
 FD  NFTENMS            LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS   OF        XFDLIB
     JOINING  TEN       AS        PREFIX.
*作場マスタ
 FD  SAKUMS.
     COPY     SAKUBAF   OF        XFDLIB
     JOINING  SAK       AS        PREFIX.
*担当者マスタ
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*ナフコ商品マスタ
 FD  NFSHOMS.
     COPY     NFSHOMS        OF        XFDLIB
     JOINING  MEI            AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  TRHAKOF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFTENMS-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  SAKUBAPF-INV-FLG        PIC  X(03)     VALUE  ZERO.
 01  HTANMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  NFSHOMS-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(08)     VALUE  137607.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  THK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3847L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3847L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3847L".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SSY3847L".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  N(20)     VALUE
         NC"＜本発ＥＸＣＥＬ確定ＤＴ取込結果リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(21)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-MS         PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD01.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(58)     VALUE  SPACE.
         05  FILLER          PIC  N(08)     VALUE
             NC"【エラー詳細】　".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(02)     VALUE  "1:".
         05  FILLER          PIC  N(12)     VALUE
             NC"ナフコ商品マスタ無　　　".
         05  FILLER          PIC  X(03)     VALUE  " 6:".
         05  FILLER          PIC  N(12)     VALUE
             NC"原価単価未登録　　　　　".
         05  FILLER          PIC  X(03)     VALUE  "11:".
         05  FILLER          PIC  N(12)     VALUE
             NC"未使用　　　　　　　　　".
 01  HD02.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(71)     VALUE  SPACE.
         05  FILLER          PIC  X(02)     VALUE  "2:".
         05  FILLER          PIC  N(12)     VALUE
             NC"商品変換ＴＢＬ無　　　　".
         05  FILLER          PIC  X(03)     VALUE  " 7:".
         05  FILLER          PIC  N(12)     VALUE
             NC"売価単価未登録　　　　　".
         05  FILLER          PIC  X(03)     VALUE  "12:".
         05  FILLER          PIC  N(12)     VALUE
             NC"未使用　　　　　　　　　".
 01  HD03.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(71)     VALUE  SPACE.
         05  FILLER          PIC  X(02)     VALUE  "3:".
         05  FILLER          PIC  N(12)     VALUE
             NC"取引先マスタ無　　　　　".
         05  FILLER          PIC  X(03)     VALUE  " 8:".
         05  FILLER          PIC  N(12)     VALUE
*#2020/07/07 NAV ST 日付範囲チェック追加
*************NC"未使用　　　　　　　　　".
             NC"納品日　日付エラー　　　".
*#2020/07/07 NAV ED
         05  FILLER          PIC  X(03)     VALUE  "13:".
         05  FILLER          PIC  N(12)     VALUE
             NC"未使用　　　　　　　　　".
 01  HD04.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(71)     VALUE  SPACE.
         05  FILLER          PIC  X(02)     VALUE  "4:".
         05  FILLER          PIC  N(12)     VALUE
             NC"ナフコ店舗マスタ無　　　".
         05  FILLER          PIC  X(03)     VALUE  " 9:".
         05  FILLER          PIC  N(12)     VALUE
*#2020/07/07 NAV ST 日付範囲チェック追加
*************NC"未使用　　　　　　　　　".
             NC"出荷日　日付エラー　　　".
*#2020/07/07 NAV ED 日付範囲チェック追加
         05  FILLER          PIC  X(03)     VALUE  "14:".
         05  FILLER          PIC  N(12)     VALUE
             NC"未使用　　　　　　　　　".
 01  HD05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"取込情報".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD05-TRDATE     PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD05-TRTIME     PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"部門".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD05-TRBUMON    PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"担当".
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD05-TRTANTO    PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  HD05-TRTANTONM  PIC  N(10).
         05  FILLER          PIC  X(12)     VALUE  SPACE.
         05  FILLER          PIC  X(02)     VALUE  "5:".
         05  FILLER          PIC  N(12)     VALUE
             NC"作場マスタ無　　　　　　".
         05  FILLER          PIC  X(03)     VALUE  "10:".
         05  FILLER          PIC  N(12)     VALUE
*#2020/07/07 NAV ST 日付範囲チェック追加
*************NC"未使用　　　　　　　　　".
             NC"入荷予定日　日付エラー　".
*#2020/07/07 NAV ED 日付範囲チェック追加
         05  FILLER          PIC  X(03)     VALUE  "15:".
         05  FILLER          PIC  N(12)     VALUE
             NC"箱⇔数量訂正　不整合　　".
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
             VALUE NC"＜ヘッダ＞".
*
 01  MS02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"管理番号".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"バッチ".
         05  FILLER          PIC  X(02)     VALUE  "NO".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"場所".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"店舗".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"場".
         05  FILLER          PIC  X(17)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"梱包数".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(04)     VALUE  "ｱｲﾃﾑ".
         05  FILLER          PIC  N(01)     VALUE  NC"数".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"エラー情報".
 01  MS03.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-KANRI      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-BTDATE     PIC  9(08).
         05  MS03-BTDATE-1   PIC  X(01).
         05  MS03-BTTIME     PIC  9(04).
         05  MS03-BTTIME-1   PIC  X(01).
         05  MS03-BTTORICD   PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-BASYO      PIC  X(02).
         05  MS03-BASYO-1    PIC  X(01).
         05  MS03-BASYO-2    PIC  X(02).
         05  MS03-BASYO-3    PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS03-TENPO      PIC  9(05).
         05  MS03-TENPO-1    PIC  X(01).
         05  MS03-NOUBAS     PIC  X(01).
         05  MS03-TENPO-2    PIC  X(01).
     03  FILLER         CHARACTER TYPE YB.
*********05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-TENPONM    PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-NYUYOTEI   PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-KONPOU     PIC  ZZZZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS03-AITEM      PIC  ZZZZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS03-ERRNM      PIC  N(20).
*
 01  MS04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
             VALUE NC"＜明　細＞".
*
 01  MS05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"商品情報".
         05  FILLER          PIC  X(16)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"商品名".
         05  FILLER          PIC  X(11)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"場所".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"店舗".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"場".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"出荷日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品数".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"原単価".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"売単価".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(15)     VALUE
             NC"_______________".
 01  MS06.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-INSTOA     PIC  X(08).
         05  MS06-INSTOA-H   PIC  X(01)     VALUE  SPACE.
         05  MS06-JANCD      PIC  X(13).
         05  MS06-JANCD-H    PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-SYONM      PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-BASYO      PIC  X(02).
         05  MS06-BASYO-1    PIC  X(01).
         05  MS06-BASYO-2    PIC  X(02).
         05  MS06-BASYO-3    PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS06-TENCD      PIC  9(05).
         05  MS06-TENCD-1    PIC  X(01).
         05  MS06-TENCD-2    PIC  X(01).
         05  MS06-TENCD-3    PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-SYUDT      PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-NOUDT      PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-NOUSU      PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS06-GENKA      PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS06-BAIKA      PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS06-ERR1       PIC  N(01).
         05  MS06-ERR2       PIC  N(01).
         05  MS06-ERR3       PIC  N(01).
         05  MS06-ERR4       PIC  N(01).
         05  MS06-ERR5       PIC  N(01).
         05  MS06-ERR6       PIC  N(01).
         05  MS06-ERR7       PIC  N(01).
         05  MS06-ERR8       PIC  N(01).
         05  MS06-ERR9       PIC  N(01).
         05  MS06-ERR10      PIC  N(01).
         05  MS06-ERR11      PIC  N(01).
         05  MS06-ERR12      PIC  N(01).
         05  MS06-ERR13      PIC  N(01).
         05  MS06-ERR14      PIC  N(01).
         05  MS06-ERR15      PIC  N(01).
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　取込データにエラーはありません。！！　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TRDATES       PIC   9(08).
 01  PARA-TRTIMES       PIC   9(06).
 01  PARA-TRDATEE       PIC   9(08).
 01  PARA-TRTIMEE       PIC   9(06).
 01  PARA-BUMON         PIC   X(04).
 01  PARA-TANCD         PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TRDATES
                                       PARA-TRTIMES
                                       PARA-TRDATEE
                                       PARA-TRTIMEE
                                       PARA-BUMON
                                       PARA-TANCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRKAKUF.
     MOVE      "TRKAKUL1"   TO   AB-FILE.
     MOVE      KAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRHAKOF.
     MOVE      "TRHAKOL3"   TO   AB-FILE.
     MOVE      THK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFTENMS.
     MOVE      "NFTENMS1"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUMS.
     MOVE      "SAKUBAL1"   TO   AB-FILE.
     MOVE      SAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTANMS.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
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
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     TRKAKUF  TRHAKOF
                        NFTENMS  SAKUMS   HTANMS  NFSHOMS.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
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
     ACCEPT   SYS-TIME          FROM   TIME.
*取込箱数確定ファイルスタート
     PERFORM  TRHAKOF-START-SEC.
     IF   END-FLG = "END"
***************************↓メッセージ適切でない
**********DISPLAY NC"＃＃　取込対象データ無１　＃＃"  UPON CONS
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*取込箱数確定ファイル読込
     PERFORM TRHAKOF-READ-SEC.
     IF   END-FLG = "END"
***************************↓メッセージ適切でない
**********DISPLAY NC"＃＃　取込対象データ無２　＃＃"  UPON CONS
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    取込箱数確定ファイルスタート
****************************************************************
 TRHAKOF-START-SEC          SECTION.
*
     MOVE    "TRHAKOF-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   THK-REC.
     INITIALIZE                        THK-REC.
*
     MOVE     "1"                 TO   THK-F06.
     MOVE     PARA-TRDATES        TO   THK-F02.
     MOVE     PARA-TRTIMES        TO   THK-F03.
*
     START  TRHAKOF  KEY  IS  >=           THK-F06  THK-F02
                                  THK-F03  THK-F01  THK-F121
                                  THK-F122 THK-F123 THK-F124
                                  THK-F12B THK-F126 THK-F127
                                  THK-F128
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 TRHAKOF-START-EXIT.
     EXIT.
*
****************************************************************
*    取込箱数確定ファイル読込
****************************************************************
 TRHAKOF-READ-SEC           SECTION.
*
     MOVE    "TRHAKOF-READ-SEC"   TO   S-NAME.
*
     READ     TRHAKOF  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TRHAKOF-READ-CNT.
*エラー区分チェック
     IF       THK-F06  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*取込日付チェック
     IF       THK-F02  >=  PARA-TRDATES
     AND      THK-F02  <=  PARA-TRDATEE
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRHAKOF-READ-EXIT
     END-IF.
*取込時刻チェック
     IF       THK-F03  >=  PARA-TRTIMES
     AND      THK-F03  <=  PARA-TRTIMEE
              CONTINUE
     ELSE
              GO                  TO   TRHAKOF-READ-SEC
     END-IF.
*
 TRHAKOF-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*取込日付／取込時刻が変わった時、改頁する。
     IF   THK-F02   =  WK-TRDATE
     AND  THK-F03   =  WK-TRTIME
          CONTINUE
     ELSE
          MOVE  THK-F02         TO   WK-TRDATE
          MOVE  THK-F03         TO   WK-TRTIME
**********ヘッダ行印字
          PERFORM HEAD-WT-SEC
     END-IF.
*
     MOVE    SPACE           TO     SUTE-FLG.
     PERFORM  TRKAKUF-START-SEC.
*数量確定ファイル更新
     PERFORM TRKAKUF-WT-SEC  UNTIL  SUTE-FLG = "END".
*
 MAIN-010.
     PERFORM  TRHAKOF-READ-SEC.
*
     IF   END-FLG  NOT =  "END"
     AND  THK-F02      =  WK-TRDATE
     AND  THK-F03      =  WK-TRTIME
          PERFORM  HEAD-WT1-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　取込数量確定ファイル　スタート
****************************************************************
 TRKAKUF-START-SEC     SECTION.
*
     MOVE    "TRKAKUF-READ-SEC"  TO   S-NAME.
*
     MOVE     SPACE              TO   KAK-REC.
     INITIALIZE                       KAK-REC.
*
     MOVE     THK-F11            TO   KAK-F10.
     MOVE     THK-F06            TO   KAK-F06.
     MOVE     THK-F01            TO   KAK-F01.
     MOVE     THK-F02            TO   KAK-F02.
     MOVE     THK-F03            TO   KAK-F03.
     MOVE     THK-F121           TO   KAK-F110.
     MOVE     THK-F122           TO   KAK-F111.
     MOVE     THK-F123           TO   KAK-F112.
     MOVE     THK-F124           TO   KAK-F113.
     MOVE     THK-F12B           TO   KAK-F11H.
     MOVE     THK-F126           TO   KAK-F115.
     MOVE     THK-F127           TO   KAK-F116.
     MOVE     THK-F128           TO   KAK-F119.
*
     START  TRKAKUF  KEY  IS  >=                    KAK-F01
                                  KAK-F02  KAK-F03  KAK-F110
                                  KAK-F111 KAK-F112 KAK-F113
                                  KAK-F11H KAK-F115 KAK-F116
                                  KAK-F119 KAK-F11A KAK-F11B
           INVALID
           MOVE  "END"           TO      SUTE-FLG
           GO                    TO      TRKAKUF-START-EXIT
     END-START.
*取込数量確定ファイル読込
     PERFORM  TRKAKUF-READ-SEC.
*
 TRKAKUF-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　取込数量確定ファイル読込
****************************************************************
 TRKAKUF-READ-SEC            SECTION.
*
     MOVE    "TRKAKUF-READ-SEC" TO        S-NAME.
*
     READ  TRKAKUF  AT  END
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TRKAKUF-READ-CNT.
*対象データチェック
 READ-030.
     IF    THK-F01  =   KAK-F01
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-040.
     IF    THK-F02  =   KAK-F02
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-050.
     IF    THK-F03  =   KAK-F03
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-060.
     IF    THK-F121 =   KAK-F110
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-070.
     IF    THK-F122 =   KAK-F111
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-080.
     IF    THK-F123 =   KAK-F112
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-090.
     IF    THK-F124 =   KAK-F113
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-100.
     IF    THK-F12B =   KAK-F11H
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-110.
     IF    THK-F126 =   KAK-F115
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-120.
     IF    THK-F127 =   KAK-F116
           CONTINUE
     ELSE
*TEST
*    DISPLAY "THK-F127=" THK-F127 UPON CONS
*    DISPLAY "KAK-F116=" KAK-F116 UPON CONS
*TEST
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-130.
     IF    THK-F128 =   KAK-F119
           CONTINUE
     ELSE
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-IF.
 READ-135.
     IF    KAK-F06  NOT =  "1"
           GO                   TO        TRKAKUF-READ-SEC
     END-IF.
 READ-140.
*****DISPLAY "KAK-F070 = " KAK-F070 UPON CONS.
*
 TRKAKUF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　数量訂正ファイル出力                              *
****************************************************************
 TRKAKUF-WT-SEC              SECTION.
*
     MOVE    "TRKAKUF-WT-SEC"   TO        S-NAME.
*
     PERFORM MEISAI-WT-SEC.
*
     PERFORM  TRKAKUF-READ-SEC.
*
 TRKAKUF-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    取込日付／時刻／部門／担当者情報セット
     MOVE     THK-F02             TO   HD05-TRDATE.
     MOVE     THK-F03             TO   HD05-TRTIME.
     MOVE     THK-F05             TO   HD05-TRBUMON.
     MOVE     THK-F04             TO   HD05-TRTANTO.
*    担当者名取得
     MOVE     PARA-BUMON          TO   TAN-F01.
     MOVE     PARA-TANCD          TO   TAN-F02.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG = SPACE
               MOVE  TAN-F03      TO   HD05-TRTANTONM
     ELSE
               MOVE  ALL NC"？"   TO   HD05-TRTANTONM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  P-LINE1 AFTER 1.
*行カウント
     MOVE     9                   TO    LINE-CNT.
*    箱数情報をセット
     MOVE     THK-F121            TO   MS03-KANRI.
     MOVE     THK-F122            TO   MS03-BTDATE.
     MOVE     "-"                 TO   MS03-BTDATE-1.
     MOVE     THK-F123            TO   MS03-BTTIME.
     MOVE     "-"                 TO   MS03-BTTIME-1.
     MOVE     THK-F124            TO   MS03-BTTORICD.
     MOVE     THK-F125            TO   MS03-BASYO.
     MOVE     "("                 TO   MS03-BASYO-1.
     MOVE     THK-F12B            TO   MS03-BASYO-2.
     MOVE     ")"                 TO   MS03-BASYO-3.
     MOVE     THK-F126            TO   MS03-TENPO.
     MOVE     "("                 TO   MS03-TENPO-1.
     MOVE     THK-F127            TO   MS03-NOUBAS.
     MOVE     ")"                 TO   MS03-TENPO-2.
     MOVE     WK-TOKCD            TO   TEN-F01.
     MOVE     THK-F126            TO   TEN-F02.
     PERFORM  NFTENMS-READ-SEC.
     IF   NFTENMS-INV-FLG = SPACE
              MOVE  TEN-F05       TO   MS03-TENPONM
     ELSE
              MOVE  ALL NC"？"    TO   MS03-TENPONM
     END-IF.
     MOVE     THK-F128            TO   MS03-NYUYOTEI.
     MOVE     THK-F129            TO   MS03-KONPOU.
     MOVE     THK-F12A            TO   MS03-AITEM.
     MOVE     NC"【取込処理でエラーがあります。確認！！】"
                                  TO   MS03-ERRNM.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  MS03  AFTER  1.
     WRITE    PRT-REC       FROM  MS04  AFTER  1.
     WRITE    PRT-REC       FROM  MS05  AFTER  1.
*行カウント
     ADD      5                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT1-SEC           SECTION.
     MOVE    "HEAD-WT1-SEC"       TO   S-NAME.
*    改頁判定
     IF       LINE-CNT  >   55
              PERFORM HEAD-WT-SEC
     END-IF.
*    箱数情報をセット
     MOVE     THK-F121            TO   MS03-KANRI.
     MOVE     THK-F122            TO   MS03-BTDATE.
     MOVE     "-"                 TO   MS03-BTDATE-1.
     MOVE     THK-F123            TO   MS03-BTTIME.
     MOVE     "-"                 TO   MS03-BTTIME-1.
     MOVE     THK-F124            TO   MS03-BTTORICD.
     MOVE     THK-F125            TO   MS03-BASYO.
     MOVE     "("                 TO   MS03-BASYO-1.
     MOVE     THK-F12B            TO   MS03-BASYO-2.
     MOVE     ")"                 TO   MS03-BASYO-3.
     MOVE     THK-F126            TO   MS03-TENPO.
     MOVE     "("                 TO   MS03-TENPO-1.
     MOVE     THK-F127            TO   MS03-NOUBAS.
     MOVE     ")"                 TO   MS03-TENPO-2.
     MOVE     WK-TOKCD            TO   TEN-F01.
     MOVE     THK-F126            TO   TEN-F02.
     PERFORM  NFTENMS-READ-SEC.
     IF   NFTENMS-INV-FLG = SPACE
              MOVE  TEN-F05       TO   MS03-TENPONM
     ELSE
              MOVE  ALL NC"？"    TO   MS03-TENPONM
     END-IF.
     MOVE     THK-F128            TO   MS03-NYUYOTEI.
     MOVE     THK-F129            TO   MS03-KONPOU.
     MOVE     THK-F12A            TO   MS03-AITEM.
     MOVE     NC"【取込処理でエラーがあります。確認！！】"
                                  TO   MS03-ERRNM.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  P-LINE1 AFTER  1.
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  MS03  AFTER  1.
     WRITE    PRT-REC       FROM  MS04  AFTER  1.
     WRITE    PRT-REC       FROM  MS05  AFTER  1.
*行カウント
     ADD      6                   TO    LINE-CNT.
*
 HEAD-WT1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    箱数情報印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*    改頁判定
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*    数量確定情報をセット
     MOVE     KAK-F11C            TO   MS06-INSTOA.
     MOVE     "("                 TO   MS06-INSTOA-H.
     MOVE     KAK-F11D            TO   MS06-JANCD.
     MOVE     ")"                 TO   MS06-JANCD-H.
*    商品名称取得
     MOVE     KAK-F11C            TO   MEI-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG = SPACE
              MOVE  MEI-F05       TO   MS06-SYONM
     ELSE
              MOVE  ALL NC"？"    TO   MS06-SYONM
     END-IF.
     MOVE     KAK-F114            TO   MS06-BASYO.
     MOVE     "("                 TO   MS06-BASYO-1.
     MOVE     KAK-F11H            TO   MS06-BASYO-2.
     MOVE     ")"                 TO   MS06-BASYO-3.
     MOVE     KAK-F115            TO   MS06-TENCD.
     MOVE     "("                 TO   MS06-TENCD-1.
     MOVE     KAK-F116            TO   MS06-TENCD-2.
     MOVE     ")"                 TO   MS06-TENCD-3.
     MOVE     KAK-F118            TO   MS06-SYUDT.
     MOVE     KAK-F117            TO   MS06-NOUDT.
     MOVE     KAK-F11E            TO   MS06-NOUSU.
     MOVE     KAK-F11J            TO   MS06-GENKA.
     MOVE     KAK-F11K            TO   MS06-BAIKA.
*    エラー区分セット
     IF  KAK-F070 = ZERO
              MOVE SPACE          TO   MS06-ERR1
     ELSE
              MOVE NC"×"         TO   MS06-ERR1
     END-IF.
*    エラー区分セット
     IF  KAK-F071 = ZERO
              MOVE SPACE          TO   MS06-ERR2
     ELSE
              MOVE NC"×"         TO   MS06-ERR2
     END-IF.
*    エラー区分セット
     IF  KAK-F072 = ZERO
              MOVE SPACE          TO   MS06-ERR3
     ELSE
              MOVE NC"×"         TO   MS06-ERR3
     END-IF.
*    エラー区分セット
     IF  KAK-F073 = ZERO
              MOVE SPACE          TO   MS06-ERR4
     ELSE
              MOVE NC"×"         TO   MS06-ERR4
     END-IF.
*    エラー区分セット
     IF  KAK-F074 = ZERO
              MOVE SPACE          TO   MS06-ERR5
     ELSE
              MOVE NC"×"         TO   MS06-ERR5
     END-IF.
*    エラー区分セット
     IF  KAK-F075 = ZERO
              MOVE SPACE          TO   MS06-ERR6
     ELSE
              MOVE NC"×"         TO   MS06-ERR6
     END-IF.
*    エラー区分セット
     IF  KAK-F076 = ZERO
              MOVE SPACE          TO   MS06-ERR7
     ELSE
              MOVE NC"×"         TO   MS06-ERR7
     END-IF.
*    エラー区分セット
     IF  KAK-F077 = ZERO
              MOVE SPACE          TO   MS06-ERR8
     ELSE
              MOVE NC"×"         TO   MS06-ERR8
     END-IF.
*    エラー区分セット
     IF  KAK-F078 = ZERO
              MOVE SPACE          TO   MS06-ERR9
     ELSE
              MOVE NC"×"         TO   MS06-ERR9
     END-IF.
*    エラー区分セット
     IF  KAK-F079 = ZERO
              MOVE SPACE          TO   MS06-ERR10
     ELSE
              MOVE NC"×"         TO   MS06-ERR10
     END-IF.
*    エラー区分セット
     IF  KAK-F07A = ZERO
              MOVE SPACE          TO   MS06-ERR11
     ELSE
              MOVE NC"×"         TO   MS06-ERR11
     END-IF.
*    エラー区分セット
     IF  KAK-F07B = ZERO
              MOVE SPACE          TO   MS06-ERR12
     ELSE
              MOVE NC"×"         TO   MS06-ERR12
     END-IF.
*    エラー区分セット
     IF  KAK-F07C = ZERO
              MOVE SPACE          TO   MS06-ERR13
     ELSE
              MOVE NC"×"         TO   MS06-ERR13
     END-IF.
*    エラー区分セット
     IF  KAK-F07D = ZERO
              MOVE SPACE          TO   MS06-ERR14
     ELSE
              MOVE NC"×"         TO   MS06-ERR14
     END-IF.
*    エラー区分セット
     IF  KAK-F07E = ZERO
              MOVE SPACE          TO   MS06-ERR15
     ELSE
              MOVE NC"×"         TO   MS06-ERR15
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  MS06  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*取込箱数確定ファイル読込
     DISPLAY "TRHAKOF   READ CNT = " TRHAKOF-READ-CNT UPON CONS.
*取込数量確定ファイル読込
     DISPLAY "TRKAKUF   READ CNT = " TRKAKUF-READ-CNT UPON CONS.
*
     CLOSE     TRKAKUF  TRHAKOF  NFTENMS  SAKUMS  HTANMS  PRTF
               NFSHOMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*               店舗　　 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 NFTENMS-READ-SEC            SECTION.
     MOVE "NFTENMS-READ-SEC"      TO   S-NAME.
*
     READ     NFTENMS
         INVALID       MOVE "INV" TO   NFTENMS-INV-FLG
         NOT  INVALID  MOVE SPACE TO   NFTENMS-INV-FLG
     END-READ.
*
 NFTENMS-READ-EXIT.
     EXIT.
****************************************************************
*               作場　　 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 SAKUBAPF-READ-SEC           SECTION.
     MOVE "SAKUBAPF-READ-SEC"     TO   S-NAME.
*
     READ     SAKUMS
         INVALID       MOVE "INV" TO   SAKUBAPF-INV-FLG
         NOT  INVALID  MOVE SPACE TO   SAKUBAPF-INV-FLG
     END-READ.
*
 SAKUBAPF-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 HTANMS-READ-SEC           SECTION.
     MOVE "HTANMS-READ-SEC"       TO   S-NAME.
*
     READ       HTANMS
         INVALID       MOVE "INV" TO   HTANMS-INV-FLG
         NOT  INVALID  MOVE SPACE TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品マスタ索引
****************************************************************
 NFSHOMS-READ-SEC          SECTION.
     MOVE "NFSHOMS-READ-SEC"      TO   S-NAME.
*
     READ       NFSHOMS
         INVALID       MOVE "INV" TO   NFSHOMS-INV-FLG
         NOT  INVALID  MOVE SPACE TO   NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
